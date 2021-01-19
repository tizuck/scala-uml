package rewriting.collectingStrats

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.{ClassPretty, UMLUnitPretty}
import scalameta.toplevel.SourcesCollector
import uml.UMLElement
import uml.strategies.collecting.CollectStrategy
import uml.strategies.collecting.packagerep.CollectNamespaceObjectsStrat
import uml.strategies.predef.{Id, NonCollect}
import uml.strategies.rewriting.InsertNamespaceObjectRelsStrat
import uml.strategies.rewriting.packagerep.DeleteInnerAssocStrat

import scala.meta.{Source, dialects}

class CollectingSuite extends AnyFreeSpec with Matchers {
  val fooAst: String =
    """
      |package foo
      |
      |import model._
      |
      |case class Model(ast:AST[T])
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

  case class TestData(program:String,nameOfFile:String) {
    val parsed = dialects.Dotty(program).parse[Source].get

    val collected = SourcesCollector(List((parsed,nameOfFile)),nameOfFile)

    val umlUnit = collected.umlUnit
  }
  "Rewriting the ast" - {
    "by collecting" - {
      "the namespace objects of fooAst yields 2 object entries" in new TestData(fooAst, "foo.scala") {
        val res = umlUnit.rewrite(Id[List[uml.Class]])(Nil)(CollectNamespaceObjectsStrat).value._1
        res must have size 2
        res.exists(c => c.name.equals("model")) must be(true)
        res.exists(c => c.name.equals("ops")) must be(true)
      }
    }
    "by deleting all inner relationships of inner objects deletes all inner relationships" in new TestData(fooAst,"foo.scala") {
      val collectedNamespaceObjects = umlUnit.rewrite(Id[List[uml.Class]])(Nil)(CollectNamespaceObjectsStrat).value._1
      println(collectedNamespaceObjects)
      val res = umlUnit.rewrite(DeleteInnerAssocStrat)(collectedNamespaceObjects)((v1: UMLElement, v2: List[uml.Class]) => v2)
      //Test: All elements except the
    }
    "by inserting new relationships " in new TestData(fooAst,"foo.scala") {
      implicit val plant = UMLUnitPretty()(PlantUMLConfig())
      println(umlUnit.pretty)
      val collectedNamespaceObjects = umlUnit
        .rewrite(Id[List[uml.Class]])(Nil)(CollectNamespaceObjectsStrat).value._1

      val delInner = umlUnit
        .rewrite(DeleteInnerAssocStrat)(collectedNamespaceObjects)((v1: UMLElement, v2: List[uml.Class]) => v2).value._2

    }
  }
}
