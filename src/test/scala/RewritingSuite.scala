import cats.data.State
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scalameta.toplevel.SourcesCollector
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{Stereotype, UMLElement}

import scala.meta.{Source, dialects}

class RewritingSuite extends AnyFreeSpec with Matchers {
  case class TestData(){
    val program =
      """
        |package foo
        |
        |trait Foo(x:Int,y:Int)
        |
        |sealed case class Bar(d:Double) extends Bar(5,5) {
        |  def bar(ints:List[Int])(using c:Context):Foo = {
        |   c.getContextFoo(ints)
        |  }
        |}
        |
        |
        |""".stripMargin

    val parsedProgram = dialects.Dotty(program).parse[Source].get

    val collectedUml = SourcesCollector(List((parsedProgram,"fooAst.scala")),"foo-ast")

  }

  "Rewriting the fooAst" - {
    "without intended changes doesn't change the fooAst" in new TestData() {
      val s : Strategy = id
      val f: (UMLElement,()) => () = (ue:UMLElement,_) => ue match {
        case _ => ()
      }
      val res = collectedUml.umlUnit.rewrite(s)(())(f)

      res.value._2 must equal(collectedUml.umlUnit)
    }

    "by collecting the trait instances yields exactly one trait Foo " in new TestData() {
      val s : Strategy = id
      val f : (UMLElement,List[String]) => List[String] = (ue,acc) => ue match {
        case c:uml.Class if c.stereotype.contains(Stereotype("trait",Nil)) => acc ++ List(c.identifier)
        case _ => acc
      }
      val res = collectedUml.umlUnit.rewrite(s)(List.empty[String])(f)

      res.value._1 must equal(List("Foo"))
    }
  }

}
