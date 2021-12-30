package collector

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalameta.types.TargetTypeCollector
import scalameta.util.context.{CollectorContext, GlobalContext, LocalContext}
import scalameta.util.namespaces.DefaultNamespace
import uml.{RefName, RefTemplate}

import scala.meta.{Term, Type}

class TypeCollectorSpec extends AnyFlatSpec with Matchers {
  implicit val context: CollectorContext =
    CollectorContext(LocalContext(),GlobalContext(Map.empty))

  "A Type of the form Type.Select" should
    "be transformed into a RefPathQualifier with correct order" in {

    val tpe = Type.Select(
      Term.Select(
        Term.Select(Term.Name("a"),Term.Name("b")),
        Term.Name("c")),
      Type.Name("B")
    )

    val ttc = TargetTypeCollector(tpe)

    ttc.umlType shouldBe a [uml.RefPathQualifier]
    println(ttc.umlType)
    ttc.umlType should equal (uml.RefPathQualifier(List("a","b","c"),"B"))
  }

  "Single Template type" should "be transformed properly" in {
    //List[String]
    val tpe = Type.Apply(Type.Name("List"),List(Type.Name("String")))

    val ttc = TargetTypeCollector(tpe)

    ttc.umlType should equal (
      uml.RefTemplate(uml.RefName("List",DefaultNamespace),List(uml.RefName("String",DefaultNamespace)))
    )
  }

  "Map template type" should "be transformed into a RefTemplate with two type parameters" in {
    val tpe = Type.Apply(Type.Name("Map"),List(Type.Name("String"),Type.Name("String")))

    val ttc = TargetTypeCollector(tpe)

    ttc.umlType should equal(
      uml.RefTemplate(uml.RefName("Map",DefaultNamespace),List(
        uml.RefName("String",DefaultNamespace),
        uml.RefName("String",DefaultNamespace)
      )))
  }

  "Template Type with depth n" should "be transformed correctly into a RefTemplate" in {
    def consturctType(n:Int):Type.Apply={
      if(n == 0){
        Type.Apply(Type.Name("Map"),List(Type.Name("A"),Type.Name("B")))
      }else {
        val value = consturctType(n-1)
        Type.Apply(Type.Name("Map"),List(Type.Name("A"),value))
      }
    }

    def constructUmlType(n:Int):RefTemplate = {
      if(n == 0){
        RefTemplate(
          RefName("Map",DefaultNamespace),
          List(
            RefName("A",DefaultNamespace),
            RefName("B",DefaultNamespace)
          ))
      } else {
        val value = constructUmlType(n-1)
        RefTemplate(
          RefName("Map",DefaultNamespace),
          List(
            RefName("A",DefaultNamespace),
            value
          )
        )
      }
    }

    for(i <- Range(1,100)){
      val tpe = consturctType(i)
      val check = constructUmlType(i)
      val translate = TargetTypeCollector(tpe)

      translate.umlType should equal (check)
    }
  }
}
