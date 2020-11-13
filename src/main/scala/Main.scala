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
      |package scalameta.util.namespaces.collector
      |import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEmpty, NamespaceEntry}
      |
      |import scala.meta.{Pkg, Source, Stat, Term}
      |import cats.implicits._
      |
      |case class SourcesCollector(override val resultingMap: Map[Entry, List[Stat]])
      |  extends BaseNamespaceCollector
      |
      |object SourcesCollector {
      |  def apply(sources:List[Source]): SourcesCollector = {
      |    SourcesCollector(sources.foldLeft(Map.empty[Entry,List[Stat]]){
      |      case (acc,source) =>
      |        val sourceMap = SourceCollector(source)
      |        acc |+| sourceMap.resultingMap
      |    } //All elements that result with a `NamespaceEmpty` entry on toplevel are positioned in the default package
      |      .map(tp => tp._1 match {case NamespaceEmpty => DefaultNamespace -> tp._2 case _ => tp})
      |      //Map the NamespaceEntry(List("default"),_) to the DefaultNamespace that occur due to algorithmic reason
      |      .map(tp => tp._1 match {case NamespaceEntry(List("default"),_) => DefaultNamespace -> tp._2 case _ => tp})
      |      //Remove default Pkg that occurs due to algorithmic reason
      |      .map(tp =>
      |          (tp._1,tp._2.filterNot(p => p match {case Pkg(Term.Name("default"),_) => true case _ => false}))
      |      )
      |    )
      |  }
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


