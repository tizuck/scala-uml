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
      |
      |object foo {
      |  def f[T: C1 : C2, U: C3](x: T)(using y: U, z: V): R
      |}
      |
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
  val fooSource = dialects.Dotty(foo).parse[Source].get
  println(fooSource.structure)
  //val barSource = dialects.Dotty(bar).parse[Source].get
  //println(source.structure)
  val namespaceMap =
    scalameta.
      util.
      namespaces.
      collector.
      SourcesCollector(List((fooSource,"foo.scala"),(scalaDefaults.default,"default.scala"))).resultingMap

  val umlCollector = UMLCollector(fooSource,GlobalContext(namespaceMap),path.toString)

  println(umlCollector.resultingContext.localCon.externalReferences.map(_.structure))

  val plantUMLUnit = umlCollector.plantUMLUnit



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
  println(namespaceMap
  .map{
    case (k,v) => (k,v.map(entry => (statToString(entry._1),entry._2)))
  })
}


