import scalameta.UMLCollector

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      |trait A {
      | def foo(b:B):Unit = ()
      | def bar:B = new B
      | trait B {
      |   trait C {
      |     trait D {
      |   }
      |  }
      | }
      |}
      |
      |val a1 = new A
      |val a2 = new A
      |a2.foo(a1.bar)
      |
      |""".stripMargin
  val source = dialects.Dotty(program).parse[Source].get
  println(source.structure)
  val plantUMLUnit = UMLCollector(source).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


