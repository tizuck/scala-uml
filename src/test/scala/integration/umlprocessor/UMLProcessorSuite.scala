package integration.umlprocessor

import app.frontend.exceptions.BadInputPathException
import app.frontend.processor.{EmptyProcessor, HelpProcessor, Processor, UMLDiagramProcessor}
import app.frontend._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class UMLProcessorSuite extends AnyFreeSpec with Matchers {

  case class TestData(proc:Processor)

  "starting the console application" - {
    "with the help command triggers the help processor" in new TestData(Processor(
      List(Help(None))
    )) {
      proc mustBe a[HelpProcessor]
      proc.execute()
    }
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
        a [BadInputPathException] must be thrownBy proc.execute()
      }
    }
    "with a non-valid input path" in new TestData(
      Processor(List(InputPath("path/foo/foo/foo/foo/foo")))
    ) {
      proc mustBe a[UMLDiagramProcessor]
      a [BadInputPathException] must be thrownBy proc.execute()
    }
    "with a valid input path" - {
      "and a valid output path" - {
        "without github parameter" - {
          "with a given name" - {
            "processes an UML diagram" in new TestData(
              Processor(
                List(
                  OutputPath("src/test/scala/assets/out/processor"),
                  InputPath("src/test/scala/assets/processor/ast"),
                  Name("ast")
                )
              )) {
              proc mustBe a[UMLDiagramProcessor]
              proc.execute()
            }
          }
        }
        "with a github repository" - {
          "with a given name" - {
            "processes a github repository as an UML diagram" in  new TestData(
              Processor(
                List(
                  OutputPath("src/test/scala/assets/out/processor/"),
                  Github("src/test/scala/assets/processor/github/github.conf"),
                  Name("ast")
                )
              )
            ) {
             // proc mustBe a[GithubUMLDiagramProcessor]
             // proc.execute()
            }
            "with a textual representation" in new TestData(
              Processor(
                List(
                  OutputPath("src/test/scala/assets/out/processor/"),
                  Github("src/test/scala/assets/processor/github/github.conf"),
                  Name("ast"),
                  Textual()
                )
              )
            ) {
             // proc mustBe a[GithubUMLDiagramProcessor]
             // proc.execute()
            }
          }
        }
      }
      "without an output path" - {
        "without a github path" - {
          "with a given name" - {
            "processes a UML diagram" in  new TestData(Processor(List(
              InputPath("src/test/scala/assets/processor/ast"),
              Name("ast")
            ))) {
              proc mustBe a[UMLDiagramProcessor]
              proc.execute()
            }
            "with a textual representation request" - {
              "processes a UML diagram" in new TestData(Processor(List(
                InputPath("src/test/scala/assets/processor/ast"),
                Name("ast"),
                Textual()
              ))) {
                proc mustBe a[UMLDiagramProcessor]
                proc.execute()
              }
            }
          }
        }
      }
      "with an output path to an illegal Scala file" - {
        "without a github path" - {
          "with a given name" - {
            "processes a UML diagram" in new TestData(Processor(List(
              InputPath("src/test/scala/assets/processor/fail"),
              Name("ast")
            ))) {
              proc mustBe a[UMLDiagramProcessor]
              a [BadInputPathException] must be thrownBy proc.execute()
            }
          }
        }
      }
    }
  }
}
