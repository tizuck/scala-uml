import java.io.{File, FileOutputStream}

import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import scalameta.SourceCollector
import scalameta.toplevel.SourcesCollector
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
      |
      |trait A
      |""".stripMargin

  val bar = """
    |package bar
    |
    |import foo._
    |
    |trait B extends A
    |
    |""".stripMargin

  val bar2 = """
              |package bar2
              |
              |import foo._
              |import bar._
              |
              |trait C extends A
              |
              |""".stripMargin
  /*val path = java.nio.file.Paths.get("src","main", "scala","uml", "ast.scala")
  val bytes = java.nio.file.Files.readAllBytes(path)
  val text = new String(bytes, "UTF-8")
  val input = Input.VirtualFile(path.toString, text)

  val astSource = input.parse[Source].get*/
  val fooSource = dialects.Dotty(foo).parse[Source].get
  val barSource = dialects.Dotty(bar).parse[Source].get
  val bar2Source = dialects.Dotty(bar2).parse[Source].get


  val umlCollector = SourcesCollector(List((fooSource,"foo.scala"),(barSource,"bar.scala"),(bar2Source,"bar2.scala")),"foobar")

  val plantUMLUnit = umlCollector.umlUnit



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
 // println("-----------------------------------------")
 // println(plantUMLUnit.pretty)
}


