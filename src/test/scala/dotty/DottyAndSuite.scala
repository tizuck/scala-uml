package dotty

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import plantuml.SimplePlantUMLPrettyPrinter
import scalameta.UMLCollector
import scalameta.util.context.GlobalContext
import uml.{Attribute, Class, GenericParameter, Operation, Parameter, UMLUnit}

import scala.meta.{Source, dialects}

class DottyAndSuite extends AnyFreeSpec with Matchers {

  object testdata {
    val _1 =
      """
        |trait Resettable {
        |  def reset(): Unit
        |}
        |trait Growable[T] {
        |  def add(t: T): Unit
        |}
        |def f(x: Resettable & Growable[String]) = {
        |  x.reset()
        |  x.add("first")
        |}
        |""".stripMargin

    val _2 =
      """trait A {
        |  def children: List[A]
        |}
        |trait B {
        |  def children: List[B]
        |}
        |val x: A & B = new C
        |val ys: List[A & B] = x.children
        |""".stripMargin

    val _1_must: UMLUnit = UMLUnit("need_to_find_id",List(Class(true,"Resettable",List(),
      List(Operation(None,None,"reset",List(List()),Some("Unit"),None)),List(),None,Some("trait")),
      Class(true,"Growable",List(),List(Operation(None,None,"add",List(List(Parameter("t","T",None))),
        Some("Unit"),None)),List(),Some(List(GenericParameter("T",None,None))),Some("trait")),
      Class(false,"F",List(),List(Operation(None,None,"f",
        List(List(Parameter("x","&<Resettable,Growable<String>>",None))),None,None)),List(),None,Some("def"))))

    val _2_must: UMLUnit = UMLUnit("need_to_find_id",List(Class(true,"A",List(),
      List(Operation(None,None,"children",List(List()),Some("List<A>"),None)),List(),None,
      Some("trait")),Class(true,"B",List(),List(Operation(None,None,"children",List(List()),
      Some("List<B>"),None)),List(),None,Some("trait")),Class(false,"X",
      List(Attribute(None,None,"x",Some("&<A,B>"),None)),List(),List(),None,Some("val")),
      Class(false,"YS",List(Attribute(None,None,"ys",Some("List<&<A,B>>"),None)),List(),List(),None,Some("val"))))


  }

  class TestData(val dottyUMLCollector : UMLCollector)

  "Dotty Reference to Intersectiontypes" - {
    "defines two traits" - {
      "Resettable and Growable that are then used as Intersection in method parameter" in
        new TestData(UMLCollector(dialects.Dotty(testdata._1).parse[Source].get, GlobalContext(Map.empty))) {
          dottyUMLCollector.plantUMLUnit must equal(testdata._1_must)
        }
      "A and B and provides vals that are intersection type of A and B" in
        new TestData(UMLCollector(dialects.Dotty(testdata._2).parse[Source].get, GlobalContext(Map.empty))) {
          dottyUMLCollector.plantUMLUnit must equal(testdata._2_must)
        }
    }
  }
}
