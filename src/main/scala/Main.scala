import scalameta.UMLCollector
import scalameta.util.context.GlobalContext
import scalameta.util.namespaces.scalaDefaults
import scalameta.util.util.statToString

import scala.meta.Defn.{Class, Object, Trait}
import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      |package uml
      |
      |trait A
      |trait B
      |trait C
      |
      |class E
      |
      |package deep {
      | trait A extends B
      |}
      |""".stripMargin
  //val path = java.nio.file.Paths.get("src","main", "scala","uml", "ast.scala")
  //println(path.toAbsolutePath)
  //val bytes = java.nio.file.Files.readAllBytes(path)
  //val text = new String(bytes, "UTF-8")
  //val input = Input.VirtualFile(path.toString, text)

  val source = dialects.Dotty(program).parse[Source].get
  //println(source.structure)
  val namespaceMap = scalameta.util.namespaces.collector.SourcesCollector(List(source,scalaDefaults.default)).resultingMap
  val plantUMLUnit = UMLCollector(source,GlobalContext(namespaceMap)).plantUMLUnit
  //println(plantUMLUnit)
  //println(plantUMLUnit.structure)
  println(plantUMLUnit.pretty)
  println("-----------------------------------------")
  println(namespaceMap
  .map{
    case (k,v) => (k,v.map(statToString))
  })
}


