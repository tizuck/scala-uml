import scalameta.UMLCollector
import scalameta.util.context.GlobalContext
import scalameta.util.util.statToString

import scala.meta.Defn.{Class, Object, Trait}
import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      |package main.scala.uml
      |
      |sealed trait UMLElement { self =>
      |  def pretty : String = SimplePlantUMLPrettyPrinter.format(self).layout
      |}
      |
      |trait StereotypeElement extends UMLElement {
      |  val stereotype: Option[String]
      |}
      |
      |sealed trait TopLevelElement extends UMLElement
      |package foo {
      |sealed trait CompartmentElement extends UMLElement
      |
      |sealed trait PackageBodyElement extends UMLElement
      |
      |sealed trait RelateableElement extends UMLElement
      |
      |sealed trait NamedElement extends UMLElement {
      |  val identifier : String
      |}
      |}""".stripMargin
  val path = java.nio.file.Paths.get("src","main", "scala","uml", "ast.scala")
  println(path.toAbsolutePath)
  val bytes = java.nio.file.Files.readAllBytes(path)
  val text = new String(bytes, "UTF-8")
  val input = Input.VirtualFile(path.toString, text)

  val source = dialects.Dotty(input).parse[Source].get
  println(source.structure)
  //val plantUMLUnit = UMLCollector(source,GlobalContext(Map.empty)).plantUMLUnit
  val namespaceMap = scalameta.util.namespaces.collector.SourceCollector(source).resultingMap
  //println(plantUMLUnit)
  //println(plantUMLUnit.structure)
  //println(plantUMLUnit.pretty)
  println("-----------------------------------------")
  println(namespaceMap
  .map{
    case (k,v) => (k,v.map(statToString))
  })
}


