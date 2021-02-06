package rewriting.collectingStrats

import cats.data.State
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import scalameta.util.namespaces.{Entry, NamespaceEntry}
import uml.strategies.collecting.CollectStrategy
import uml.strategies.collecting.packagerep.{CollectAllClassesStrat, CollectAllNamespacesStrat, CollectNamespaceObjectsStrat}
import uml.strategies.predef.Id
import uml.strategies.rewriting.RewriteStrategy
import uml.strategies.rewriting.packagerep._
import uml.{Inner, Relationship, UMLElement, UMLUnit}

import scala.meta.{Source, dialects}

class PackageRepresentationSuite extends AnyFreeSpec with Matchers {
  val program: String =
    """
      |package foo
      |
      |import foo.model._
      |
      |case class Model[T]() extends AST[T]{
      |}
      |
      |object model {
      | trait AST[T]
      | sealed case class ASTNode[T](nodes:List[AST[T]],entry:T) extends AST[T]
      | sealed case class ASTLeave[T](entry:T) extends AST[T]
      |}
      |
      |object ops {
      | def fooAST[T](ast:AST[T]):AST[T] = throw new NotImplementedError
      |}
      |
      |""".stripMargin

  val parsed: Source = dialects.Scala3(program).parse[Source].get

  val collected: SourcesCollector = SourcesCollector(List((parsed,"foo.scala")),"foo")

  val umlUnit: UMLUnit = collected.umlUnit

  "during the transformation-step from toplevel-representation to package-representation " - {
    "the first step deletes all inner relationships between objects and namespaces" in {

      val res = umlUnit.rewrite(Id[List[uml.Class]]())(List.empty[uml.Class])(CollectNamespaceObjectsStrat).value._1

      res must have size 2
      res.exists(c => c.name.equals("model")) must be(true)
      res.exists(c => c.name.equals("ops")) must be(true)

      val res2 = umlUnit.rewrite(DeleteInnerAssocStrat)(res)((_: UMLElement, v2: List[uml.Class]) => v2).value._2

      res2.count({
        case uml.Relationship(uml.Inner,_,_,_) => true
        case _ => false
      }) must equal(0)
    }
    "the second step introduces all packages without content in the UML tree " in {
      val collectedNamespaceObjects = umlUnit
        .rewrite(Id[List[uml.Class]]())(List.empty[uml.Class])(CollectNamespaceObjectsStrat)
        .value
        ._1

      val deletedInner = umlUnit
        .rewrite(DeleteInnerAssocStrat)(collectedNamespaceObjects)((_: UMLElement, v2: List[uml.Class]) => v2)
        .value
        ._2

      val allNamespaces = deletedInner
        .rewrite(Id[List[Entry]]())(List.empty[Entry])(CollectAllNamespacesStrat)
        .value
        ._1

      allNamespaces.contains(NamespaceEntry(List("foo"))) must be(true)
      allNamespaces.contains(NamespaceEntry(List("foo","model"))) must be(true)
      allNamespaces must have size 2

      val withNewPackages = deletedInner
        .rewrite(InsertPackagesFromNamespacesStrat)(allNamespaces)((_: UMLElement, v2: List[Entry]) => v2)
        .value
        ._2

      //The amount of packages inserted matches expected amount
      withNewPackages.count({
        case _:uml.Package => true
        case _ => false
      }) must equal(2)

      //correct packages are inserted
      allNamespaces
        .forall(e => withNewPackages
          .exists({
            case uml.Package(_,_,namespace) => namespace.equals(e)
            case _ => false
          })) must be(true)
    }
    "the third step shifts all classes from the toplevel into the packages, " +
      "so that there is no class or relationship without a package except for the definitions in" +
      "default namespace" in {
      val collectedNamespaceObjects = umlUnit
        .rewrite(Id[List[uml.Class]]())(List.empty[uml.Class])(CollectNamespaceObjectsStrat)
        .value
        ._1

      val deletedInner = umlUnit
        .rewrite(DeleteInnerAssocStrat)(collectedNamespaceObjects)((_: UMLElement, v2: List[uml.Class]) => v2)
        .value
        ._2

      val allNamespaces = deletedInner
        .rewrite(Id[List[Entry]]())(List.empty[Entry])(CollectAllNamespacesStrat)
        .value
        ._1

      val withNewPackages = deletedInner
        .rewrite(InsertPackagesFromNamespacesStrat)(allNamespaces)((_: UMLElement, v2: List[Entry]) => v2)
        .value
        ._2

      val allClassesCollected = withNewPackages
        .rewrite(Id[List[uml.Class]]())(List.empty)(CollectAllClassesStrat)
        .value
        ._1

      allClassesCollected must have size 6
      allClassesCollected.forall{
        c => c.name.equals("Model") ||
          c.name.equals("AST") ||
          c.name.equals("ASTNode") ||
          c.name.equals("ASTLeave") ||
          c.name.equals("ops") ||
          c.name.equals("model")
      }

      val sortInClasses = withNewPackages
        .rewrite(InsertClassesInPackageStrat)(allClassesCollected)((_: UMLElement, v2: List[uml.Class]) => v2)
        .value
        ._2.asInstanceOf[UMLUnit]

      val deletedClasses = sortInClasses
        .rewrite(DeleteAllClassesOnToplevel)(allClassesCollected)((_: UMLElement, v2: List[uml.Class]) => v2)
        .value
        ._2.asInstanceOf[UMLUnit]

      implicit val pretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
      //There are only packages and relationships now on toplevel, since no definition of an entity is on default level
      deletedClasses.forall({
        case u@UMLUnit(_, _) =>
          u.toplevelElements.forall({
            case _:Relationship => true
            case _:uml.Package => true
            case _ => false
          })
        case _ => true
      }) must be(true)
    }
    "the last step inserts an inner relationship with stereotype objectdef to every object that defines elements" +
      "in a namespace" in {
      implicit val pretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

      def startState[T](start:T):State[UMLElement,T] = State(
        umlElem => (umlElem,start)
      )

      def nextRewriteState[T](start:T)(rewriteStrategy: RewriteStrategy[T]):State[UMLElement,T] = State(
        umlElem => (umlElem.rewrite(rewriteStrategy)(start)((_,t) => t).value._2,start)
      )

      import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

      def nextCollectState[T](start:T)(collectStrategy: CollectStrategy[T]):State[UMLElement,T] = State(
        umlElem => (umlElem,umlElem.rewrite((_:T) => id)(start)(collectStrategy).value._1)
      )


       val transferToPackageRep: State[UMLElement,List[uml.Class]] =
        for {
          start <- startState(List.empty[uml.Class])
          collected <- nextCollectState(start)(CollectNamespaceObjectsStrat)
          _ <- nextRewriteState(collected)(DeleteInnerAssocStrat)
          namespace <- nextCollectState(List.empty[Entry])(CollectAllNamespacesStrat)
          _ <- nextRewriteState(namespace)(InsertPackagesFromNamespacesStrat)
          allClasses <- nextCollectState(List.empty[uml.Class])(CollectAllClassesStrat)
          _ <- nextRewriteState(allClasses)(InsertClassesInPackageStrat)
          _ <- nextRewriteState(allClasses)(DeleteAllClassesOnToplevel)
          _ <- nextRewriteState(())(DeleteEmptyPackages)
          res <- nextRewriteState(collected)(InsertInnerNamespaceRelsStrat)
        } yield {
          res
        }

      val rewrittenUmlUnit = transferToPackageRep.run(umlUnit).value._1.asInstanceOf[UMLUnit]

      rewrittenUmlUnit.count{
        case UMLUnit(_, toplevelElements) =>
          toplevelElements.exists({case Relationship(Inner,_,_,_) => true case _ => false})
        case _ => false
      } must be (1)

    }
  }
}
