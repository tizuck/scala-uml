package uml

import cats.Eval
import cats.data.State
import scalameta.util.namespaces.Entry
import uml.UMLElement
import uml.externalReferences.ClassDefRef
import uml.strategies.collecting.CollectStrategy
import uml.strategies.collecting.assoc.{CollectAllAssociationsBasedOn, CollectAllClassDefRefs}
import uml.strategies.collecting.packagerep.{CollectAllClassesStrat, CollectAllNamespacesStrat, CollectNamespaceObjectsStrat}
import uml.strategies.rewriting.assoc.TransformAssociations
import uml.strategies.rewriting.{DistinctionStrat, RewriteStrategy}
import uml.strategies.rewriting.packagerep.{DeleteAllClassesOnToplevel, DeleteEmptyPackages, DeleteInnerAssocStrat, InsertClassesInPackageStrat, InsertInnerNamespaceRelsStrat, InsertPackagesFromNamespacesStrat}
import uml.strategies.rewriting.companion.{InsertCompanionDependency, RenameAllAffectedRelationships, RenameCompanionObject}
object umlMethods {

  private def startState[T](start:T):State[UMLElement,T] = State(
    umlElem => (umlElem,start)
  )

  private def nextRewriteState[T](start:T)(rewriteStrategy: RewriteStrategy[T]):State[UMLElement,T] = State(
    umlElem => (umlElem.rewrite(rewriteStrategy)(start)((_,t) => t).value._2,start)
  )

  import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

  private def nextCollectState[T](start:T)(collectStrategy: CollectStrategy[T]):State[UMLElement,T] = State(
    umlElem => (umlElem,umlElem.rewrite((_:T) => id)(start)(collectStrategy).value._1)
  )

  private val distinctRep : State[UMLElement,Unit] =
    for {
      res <- nextRewriteState(())(DistinctionStrat)
    } yield {
      res
    }

  private val toExternalAssociationsRep : State[UMLElement,List[Relationship]] =
    for {
      allClassDefRefs <- nextCollectState(List.empty[ClassDefRef])(CollectAllClassDefRefs)
      allAffectedAssocs <- nextCollectState(List.empty[Relationship])(CollectAllAssociationsBasedOn(allClassDefRefs))
      res <- nextRewriteState(allAffectedAssocs)(TransformAssociations)
    } yield {
      res
    }

  private val transferToPackageRep: State[UMLElement,List[uml.Class]] =
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

  private val addCompanionObjects: State[UMLElement,List[(uml.Class,Boolean)]] =
    for {
      allClasses <- nextCollectState(List.empty[uml.Class])(CollectAllClassesStrat)
      allEqualClasses = classesAndCompanion(allClasses)
      _ <- nextRewriteState(allEqualClasses)(RenameCompanionObject)
      _ <- nextRewriteState(allEqualClasses)(InsertCompanionDependency)
      res <- nextRewriteState(allEqualClasses)(RenameAllAffectedRelationships)
    } yield {
      res
    }

  private def classesAndCompanion(allClasses: List[Class]) = {
    allClasses.map {
      c =>
        (c
          , allClasses.exists(
          c2 => !c.equals(c2) &&
            c.name.equals(c2.name) &&
            c.namespace.equals(c2.namespace) &&
            c2.stereotype.exists(s => s.name.equals("object"))
        )
        )
    }
  }

  def toPackageRep(umlElement: UMLElement): Eval[UMLElement] =
    transferToPackageRep.runS(umlElement)

  def insertCompanionObjects(umlUnit:UMLUnit) : Eval[UMLUnit] =
    addCompanionObjects.runS(umlUnit).asInstanceOf[Eval[UMLUnit]]

  def toDistinctRep(umlElement: UMLElement) : Eval[UMLElement] =
    distinctRep.runS(umlElement)

  def toAssocRep(umlElement: UMLElement) : Eval[UMLElement] =
    toExternalAssociationsRep.runS(umlElement)
}
