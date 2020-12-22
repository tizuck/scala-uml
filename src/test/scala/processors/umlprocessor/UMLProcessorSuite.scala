package processors.umlprocessor

import app.frontend.{Command, InputPath, OutputPath}
import app.frontend.exceptions.{BadInputPathException, BadOutputPathException}
import app.frontend.processor.{EmptyProcessor, Processor, UMLDiagramProcessor}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.slf4j.{Logger, LoggerFactory}

class UMLProcessorSuite extends AnyFreeSpec with Matchers {

  case class TestData(proc:Processor)

  "starting the console application" - {
    "without a given files path" - {
      "without a given output path" - {
        "without github parameter" - {
          "without a given name or with a given name" - {
            "processes nothing" in new TestData(Processor(List.empty[Command])) {
              proc mustBe a[EmptyProcessor.type]
            }
          }
        }
      }
      "with a given non-valid output path" in new TestData(
        Processor(List(OutputPath("path/foo/foo/foo/foo/foo")))
      ){
        proc mustBe a[UMLDiagramProcessor]
        a [BadOutputPathException] must be thrownBy proc.execute()
      }
    }
    "with a non-valid input path" in new TestData(
      Processor(List(InputPath("path/foo/foo/foo/foo/foo")))
    ) {
      proc mustBe a[UMLDiagramProcessor]
      a [BadInputPathException] must be thrownBy proc.execute()
    }
  }
}
