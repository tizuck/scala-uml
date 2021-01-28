package integration.github

import app.frontend.processor.Processor
import app.frontend.{Github, Name, OutputPath, Textual}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scalameta.util.namespaces.NamespaceEntry
import uml.UMLUnit

/**
 * Integration test rgarding the CLS framework found at:
 * (CLS types package)[https://github.com/combinators/cls-scala/blob/master/src/main/scala/org/combinators/cls/types]
 *
 * Tests are oriented towards the verification of each feature in combination.
 * Thus, all features are tested randomly once.
 *
 * Test include:
 * - sealed trait
 *   - values
 *   - operations
 *   - trait stereotype
 *   - namespace of trait
 * - companion object existence of
 *   - trait
 *   - case class
 * - case class
 *   - values
 *   - operations
 *   - secondary constructor
 *   - primary constructor
 */
class CLSSuite extends AnyFreeSpec with Matchers {
  val confPath = "src/test/scala/assets/processor/github/github.conf"
  val outputPath = "src/test/scala/assets/out/processor"
  val commands = List(Github(confPath),OutputPath(outputPath),Textual(),Name("cls"))
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

  " contains a case object Omega with a static value paths" in {
    umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("Omega") &&
          c.stereotype.exists(s => s.name.equals("caseobject")) &&
          c.attributes.exists(s => s.name.equals("paths") && s.attributeType.equals("&<Type,Path>")) &&
          c.operations.exists(o =>
            o.name.equals("toStringPrec") &&
              o.paramSeq.exists(s => s.exists(p => p.name.equals("prec") && p.paramType.equals("Int")))
          )
      case _ => false
    } must be(true)
  }

  /*" contains a trait Minimizable that contains a type declaration T" in {
    umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("Minimizable") &&
        c.stereotype.exists(s => s.name.equals("trait")) &&
        c.
    }
  }*/

  " contains all class definitions in type.scala are present" in {
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
