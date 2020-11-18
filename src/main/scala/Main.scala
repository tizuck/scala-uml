import java.io.{File, FileOutputStream}

import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import scalameta.UMLCollector
import scalameta.util.context.GlobalContext
import scalameta.util.namespaces.scalaDefaults
import scalameta.util.util.statToString

import scala.meta.Defn.{Class, Object, Trait}
import scala.meta._
import scala.meta.dialects

object Main extends App {
  val foo =
    """
      |package foo
      |trait B[+T,E] {
      |  val self:A[Int]
      |}
      |
      |trait A[T] {
      | val self:B[Nothing,Nothing]
      | def foo():Unit
      |}
      |""".stripMargin

  val bar = """
    |package foo
    |
    |import scala.util._

    |trait C extends B[Nothing,Nothing]
    |trait D extends B[Nothing,Nothing]
    |
    |""".stripMargin
  val path = java.nio.file.Paths.get("src","main", "scala","uml", "ast.scala")
  val bytes = java.nio.file.Files.readAllBytes(path)
  val text = new String(bytes, "UTF-8")
  val input = Input.VirtualFile(path.toString, text)

  val astSource = input.parse[Source].get
  //val fooSource = dialects.Dotty(foo).parse[Source].get
  //val barSource = dialects.Dotty(bar).parse[Source].get
  //println(source.structure)
  val namespaceMap =
    scalameta.
      util.
      namespaces.
      collector.
      SourcesCollector(List((astSource,path.toString),(scalaDefaults.default,"default.scala"))).resultingMap

  val plantUMLUnit = UMLCollector(astSource,GlobalContext(namespaceMap),path.toString).plantUMLUnit


  //println(plantUMLUnit)
  //println(plantUMLUnit.structure)

  //hack some skinparams into the files

  val prettyFile = plantUMLUnit
    .pretty
    .substring(0,plantUMLUnit.pretty.lastIndexOf("\n"))
    .appended('\n')
    .appendedAll(
      """
        |skinparam linetype ortho
        |
        |skinparam class {
        | Backgroundcolor white
        | Bordercolor black
        | Arrowcolor Black
        |}
        |
        |hide circle
        |@enduml
        |""".stripMargin
    )
  val reader = new SourceStringReader(prettyFile)
  val filePath = new File("diaOut")

  filePath.mkdirs()

  val fos = new FileOutputStream(new File(filePath.getPath + "/ast.svg"))
  val sec = reader.generateImage(fos,new FileFormatOption(FileFormat.SVG))
  println(plantUMLUnit.pretty)
  println("-----------------------------------------")
  println(namespaceMap
  .map{
    case (k,v) => (k,v.map(entry => (statToString(entry._1),entry._2)))
  })
}


