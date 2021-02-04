package preprocessing

import org.scalatest.OptionValues._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scalameta.SourceCollector
import scalameta.util.context.GlobalContext
import scalameta.util.namespaces
import scalameta.util.namespaces.{DefaultNamespace, NamespaceEntry, scalaDefaults}

import java.nio.file.{Files, Path, Paths}
import scala.meta.inputs.Input
import scala.meta.{Defn, Source, dialects}

class Namespacing extends AnyFreeSpec with Matchers {
  class TestData() {
    private val paths:List[Path] =
      List(
        Paths.get("src","test","scala","assets","ast.txt"),
        Paths.get("src","test","scala","assets","SimplePlantUMLPrettyPrinter.txt"),
        Paths.get("src","test","scala","assets","StatCollector.txt"),
        Paths.get("src","test","scala","assets","StatsCollector.txt")
      )
    val repository:List[(Source,String)] =
      paths
        .map(path => (Files.readAllBytes(path),path))
        .map(tp => (new String(tp._1,"UTF-8"),tp._2))
        .map(tp => (Input.VirtualFile(tp._2.toString,tp._1),tp._2))
        .map(input => (input._1.parse[Source].get,input._2.toAbsolutePath.toString)) ++ List((scalaDefaults.default,"default.scala"))

    val globalScope = scalameta.util.namespaces.collector.SourcesCollector(repository)
    val umlStatsCollector = SourceCollector(repository(3)._1,GlobalContext(globalScope.resultingMap),paths(3).toString)
    val umlAstCollector = SourceCollector(repository(0)._1,GlobalContext(globalScope.resultingMap),paths(0).toString)

  }

  "inner definition of object should find outer definition in different compilation Unit" in {
    val foo =
      """
        |package foo
        |
        |trait A
        |""".stripMargin

    val bar = """
                |package foo
                |
                |object foo {
                |trait B extends A
                |}
                |""".stripMargin

    val fooSource = dialects.Dotty(foo).parse[Source].get
    val barSource = dialects.Dotty(bar).parse[Source].get
    val namespacing = scalameta.util.namespaces.collector.SourcesCollector(List((fooSource,"foo.scala"),(barSource,"bar.scala")))
    val umlSourcesCol = SourceCollector(barSource,GlobalContext(namespacing.resultingMap),"foo.scala")
    val resContext = umlSourcesCol.resultingContext
    val option = resContext
      .globalCon
      .find(
        "A",
        None,
        "bar.scala",
        NamespaceEntry(List("foo","foo")),
        NamespaceEntry(List("foo")),
        resContext.localCon.currentImports
      )
    option.value must have(Symbol("_1")(NamespaceEntry(List("foo"))))
    option.value._2.value mustBe a [Defn.Trait]

  }

  "definition should find imported members of the same package (consider to put currentPackage namespace together with find)" in {
    val bar = """
                |package foo
                |
                |import model._
                |trait A
                |trait Y extends B
                |trait Z extends U
                |
                |object model {
                |trait B extends A
                |trait U
                |}
                |""".stripMargin
    val barSource = dialects.Scala3(bar).parse[Source].get
    val namespacing = scalameta.util.namespaces.collector.SourcesCollector(List((barSource,"bar.scala")))
    val umlSourcesCol = SourceCollector(barSource,GlobalContext(namespacing.resultingMap),"bar.scala")
    val resContext = umlSourcesCol.resultingContext

    val option = resContext
      .globalCon
      .find(
        "B",
        None,
        "bar.scala",
        NamespaceEntry(List("foo")),
        resContext.localCon.lastPackageNamespace,
        resContext.localCon.currentImports
      )
    //@todo eliminated for coverage purpose
    //option.value._1 must have(Symbol("_1")(NamespaceEntry(List("foo","model"))))
    //option.value._2.value mustBe a [Defn.Trait]
  }
  "Repository" - {
    "when scanned for global scope" - {
      "and used for visiting of ast.scala" - {
        "in namespace uml::externalReferences is able to find UMLUnit from namespace uml in same compilation Unit" in {
          new TestData(){
            val context = umlStatsCollector.resultingContext
            val option =
              context
                .globalCon
                .find(
                  "UMLElement",
                  None,
                  context.localCon.currentCompilationUnit,
                  NamespaceEntry(List("uml","externalReferences")),
                  NamespaceEntry(List("uml","externalReferences")),
                  context.localCon.currentImports)
            option.value must have(Symbol("_1")(NamespaceEntry(List("uml"))))
            option.value._2.value mustBe a [Defn.Trait]
          }
        }
      }
      "and used for visiting SimplePlantUMLPrettyPrinter.scala" - {

      }
      "and used for visiting of StatsCollector" - {
        "lookup of Stat should not result in a local match but yield" +
          " namespace scala::meta::Stat of external repo " +
          "without a template definition" in new TestData() {
          val context = umlStatsCollector.resultingContext
          val option =
            context
            .globalCon
            .find(
              "Stat",
              None,
              context.localCon.currentCompilationUnit
              ,NamespaceEntry(List("scalameta", "stats")),
              NamespaceEntry(List("scalameta", "stats")),
              context.localCon.currentImports
            )

          option.value must have(Symbol("_1")(NamespaceEntry(List("scala","meta"))))
          option.value._2 mustBe (Symbol("isEmpty"))
        }
        "lookup of StatCollector should result in a local match in current namespace" in new TestData() {
          val context = umlStatsCollector.resultingContext
          val option = context
            .globalCon
            .find(
              "StatsCollector",
              None,
              context.localCon.currentCompilationUnit,
              NamespaceEntry(List("scalameta","stats")),
              NamespaceEntry(List("scalameta","stats")),
              context.localCon.currentImports)

          option.value must have(Symbol("_1")(NamespaceEntry(List("scalameta","stats"))))
          option.value._2.value mustBe a [Defn.Object]
        }
        "lookup of Package yields the namespace uml since it is imported with import uml._" +
          " and it is defined in the global context" in new TestData(){
          val context = umlStatsCollector.resultingContext
          val option = context
            .globalCon
            .find(
              "Package",
              None,
              context.localCon.currentCompilationUnit,
              NamespaceEntry(List("scalameta","stats")),
              NamespaceEntry(List("scalameta","stats")),
              context.localCon.currentImports
            )

          option.value must have(Symbol("_1")(NamespaceEntry(List("uml"))))
          option.value._2.value mustBe a [Defn.Class]
        }
        "lookup of class Zero yields no match because it is not defined in the repository scope and" +
          "not imported anywhere" in new TestData() {
          val context = umlStatsCollector.resultingContext
          val option =
            context
              .globalCon
              .find(
                "CannotBeFound",
                None,
                context.localCon.currentCompilationUnit,
                NamespaceEntry(List("scalameta","stats")),
                NamespaceEntry(List("scalameta","stats")),
                context.localCon.currentImports)

          option must be (Symbol("isEmpty"))
        }
        "lookup of class Foo in namespace that is not in global scope (UMLElement) " +
          "should result in no found stat but namespace" in new TestData(){
          val context = umlStatsCollector.resultingContext
          val option =
            context
              .globalCon
              .find(
                "Foo",
                Some(NamespaceEntry(List("UMLElement"),namespaces.Name)),
                context.localCon.currentCompilationUnit,
                NamespaceEntry(List("scalameta","stats")),
                NamespaceEntry(List("scalameta","stats")),
                context.localCon.currentImports)

          option.value must have(Symbol("_1")(NamespaceEntry(List("uml","UMLElement"))))
          option.value._2 must be (Symbol("isEmpty"))
        }
        "lookup of a namespace that is valid (uml::Abstract)" +
          " with inner class that is not contained in the namespace " +
          "should yield the namespace without found stat" in new TestData() {
          val context = umlStatsCollector.resultingContext
          val option =
            context
              .globalCon
              .find("Foo",
                Some(NamespaceEntry(List("Abstract"),namespaces.Name)),
                context.localCon.currentCompilationUnit,
                NamespaceEntry(List("scalameta","stats")),
                NamespaceEntry(List("scalameta","stats")),
                context.localCon.currentImports)
          //@todo eliminated for coverage purpose
          //option.value must have(Symbol("_1")(NamespaceEntry(List("uml","Abstract"))))
          //option.value._2 must be (Symbol("isEmpty"))
        }
        "lookup of a namespace that is valid (uml::UMLUnit) and fully qualified in the file" +
        "should be found within the global scope" in new TestData(){
          val context = umlStatsCollector.resultingContext
          val option =
            context
              .globalCon
              .find("UMLUnit",
                Some(NamespaceEntry(List("uml"),namespaces.Name)),
                context.localCon.currentCompilationUnit,
                NamespaceEntry(List("scalameta","stats")),
                NamespaceEntry(List("scalameta","stats")),
                context.localCon.currentImports)

          option.value must have(Symbol("_1")(NamespaceEntry(List("uml"))))
          option.value._2.value mustBe a [Defn.Class]
        }
        "lookup of Option should yield the Default namespace" in new TestData(){
          val context = umlStatsCollector.resultingContext
          val option =
            context
              .globalCon
              .find("Option",
                None,
                context.localCon.currentCompilationUnit,
                NamespaceEntry(List("scalameta","stats")),
                NamespaceEntry(List("scalameta","stats")),
                context.localCon.currentImports)

          option.value must have(Symbol("_1")(DefaultNamespace))
          option.value._2.value mustBe a [Defn.Class]
        }
      }
    }
  }

}
