package integration.github

import app.frontend.{Github, InputPath, Name, OutputPath, Textual}
import app.frontend.processor.Processor
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scalameta.util.namespaces.NamespaceEntry
import uml.UMLUnit

class LocalCLSSuite extends AnyFreeSpec with Matchers {

  val inputPath = "src/test/resources/assets/processor/cls/"
  val outputPath = "src/test/resources/assets/out/processor/"
  //Not(Exclude("""(org::combinators::cls::types)|((org::combinators::cls::types.)?(\$Type|Type|Arrow|Constructor|Intersection|Omega|Product|Variable))""".r))
  val commands = List(InputPath(inputPath),OutputPath(outputPath),Textual(),Name("clsLocal"))

  //val commands = List(Github(confPath),OutputPath(outputPath),Textual(),Name("cls"),Not(Exclude("""(org::cls::combinator::types\.)?(Arrow|Constructor|Intersection|Omega|Product|Variable)""".r)))
  val processor: Processor = Processor(commands)
  val umlUnit: UMLUnit = processor.execute()

  "The resulting UML class diagram"- {
    "  contains a trait Type that is sealed and has a value isOmega and a method toStringPrec" in {

      umlUnit.exists {
        case c: uml.Class =>
          c.name.equals("Type") &&
            c.namespace.equals(NamespaceEntry(List("org", "combinators", "cls", "types"))) &&
            c.stereotype.exists(s => s.name.equals("trait")) &&
            c.additionalCompartements.exists(co => co.taggedValues.exists(t => t.name.equals("isSealed"))) &&
            c.operations.exists(o =>
              o.name.equals("toStringPrec") &&
                o.returnType.get.equals("String") &&
                o.paramSeq.exists(pl => pl.exists(p => p.name.equals("prec") && p.paramType.equals("Int")))
            )
        case _ => false
      } must be(true)
    }
  }

  " contains a companion object Type that has one method intersect" in {
    umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("$Type") &&
          c.stereotype.exists(s => s.name.equals("object")) &&
          c.operations.exists(o =>
            o.name.equals("intersect") &&
              o.returnType.exists(s => s.equals("Type")) &&
              o.paramSeq.exists(s => s.exists(p => p.name.equals("types") && p.paramType.equals("Seq<Type>")))
          )
      case _ => false
    } must be(true)
  }

  "case class constructor with secondary constructor and primary constructor" in {
    umlUnit.exists {
      case c: uml.Class =>
        c.name.equals("Constructor") &&
          c.stereotype.exists(s => s.name.equals("caseclass")) &&
          c.operations.exists(o =>
            o.name.equals("toStringPrec") &&
              o.paramSeq.exists(s => s.exists(p => p.name.equals("prec") && p.paramType.equals("Int")))
          ) &&
          c.attributes.exists(a =>
            a.name.equals("isOmega") &&
              a.attributeType.exists(t => t.equals("Boolean"))
          ) &&
          c.operations.exists(o =>
            o.stereotype.exists(s => s.name.equals("ctor")) &&
              o.paramSeq.exists(s => s.exists(p => p.name.equals("name") &&
                p.paramType.equals("String")))
          ) &&
          c.operations.exists(o =>
            o.name.equals("this") &&
              o.paramSeq.exists(s =>
                s.exists(p => p.name.equals("name") && p.paramType.equals("String")) &&
                  s.exists(p => p.name.equals("argument") && p.paramType.equals("Type")) &&
                  s.exists(p => p.name.equals("arguments") && p.paramType.equals("VarArgs<Type>"))
              )
          )
      case _ => false
    } must be(true)
  }

  " contains a companion object Constructor" in {
    umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("$Constructor") &&
          c.stereotype.exists(s => s.name.equals("object"))
      case _ => false
    } must be(true)
  }

  " contains a case object Omega with a value paths" in {
    umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("Omega") &&
          c.stereotype.exists(s => s.name.equals("caseobject")) &&
          c.attributes.exists(s => s.name.equals("paths") && s.attributeType.get.trim.equals("List<&<Type,Path>>")) &&
          c.operations.exists(o =>
            o.name.equals("toStringPrec") &&
              o.paramSeq.exists(s => s.exists(p => p.name.equals("prec") && p.paramType.equals("Int")))
          )
      case _ => false
    } must be(true)
  }

  " contains all class definitions in type.scala" in {
    umlUnit.collect {
      case c:uml.Class =>
        if(c.name.equals("Type") ||
          c.name.equals("$Type") ||
          c.name.equals("Constructor") ||
          c.name.equals("$Constructor") ||
          c.name.equals("Product") ||
          c.name.equals("Intersection") ||
          c.name.equals("Omega") ||
          c.name.equals("Arrow") ||
          c.name.equals("Variable")) {
          Some(c.name)
        } else {
          None
        }
      case _ => None
    }.flatten
      .toSet
      .equals(
        Set(
          "Type",
          "$Type",
          "Constructor",
          "$Constructor",
          "Product",
          "Intersection",
          "Omega",
          "Arrow",
          "Variable")
      ) must be(true)
  }

}