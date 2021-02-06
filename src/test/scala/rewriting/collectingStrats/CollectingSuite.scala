package rewriting.collectingStrats

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scalameta.toplevel.SourcesCollector
import uml.UMLUnit
import uml.strategies.collecting.packagerep.CollectNamespaceObjectsStrat
import uml.strategies.predef.Id

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
    val parsed: Source = dialects.Scala3(program).parse[Source].get

    val collected: SourcesCollector = SourcesCollector(List((parsed,nameOfFile)),nameOfFile)

    val umlUnit: UMLUnit = collected.umlUnit
  }
  "Rewriting the ast" - {
    "by collecting" - {
      "the namespace objects of fooAst yields 2 object entries" in new TestData(fooAst, "foo.scala") {
        val res: List[uml.Class] = umlUnit.rewrite(Id[List[uml.Class]]())(Nil)(CollectNamespaceObjectsStrat).value._1
        res must have size 2
        res.exists(c => c.name.equals("model")) must be(true)
        res.exists(c => c.name.equals("ops")) must be(true)
      }
    }
  }
}
