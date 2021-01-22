package processors.github

import app.frontend.processor.Processor
import app.frontend.{Github, Name, OutputPath, Textual}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scalameta.util.namespaces.NamespaceEntry

import scala.io.Source


class CLSSuite extends AnyFreeSpec with Matchers {
  val confPath = "src/test/scala/assets/processor/github/github.conf"
  val outputPath = "src/test/scala/assets/out/processor"
  val commands = List(Github(confPath),OutputPath(outputPath),Textual(),Name("cls"))
  val processor = Processor(commands)
  val umlUnit = processor.execute()

  "The resulting UML class diagram contains a trait called Type" +
    " that is sealed and has a value isOmega and a method toStringPrec" in {

    umlUnit.exists{
      case c:uml.Class =>
        c.name.equals("Type") &&
        c.namespace.equals(NamespaceEntry(List("org","combinators","cls","types"))) &&
        c.stereotype.exists(s => s.name.equals("trait")) &&
        c.additionalCompartements.exists(co => co.taggedValues.exists(t => t.name.equals("isSealed"))) &&
        c.operations.exists(o =>
          o.name.equals("toStringPrec") &&
            o.returnType.get.equals("String") &&
            o.paramSeq.exists(pl => pl.exists(p => p.name.equals("prec") && p.paramType.equals("Int")))
        )
      case _ => false
    }

  }
}
