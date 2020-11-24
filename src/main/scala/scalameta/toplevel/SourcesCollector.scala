package scalameta.toplevel

import scalameta.implicits._
import cats.implicits._
import scalameta.SourceCollector
import scalameta.util.context.{CollectorContext, GlobalContext}
import uml.UMLUnit

import scala.meta.Source

case class SourcesCollector(umlUnit:UMLUnit) {

}

object SourcesCollector {
  def apply(sources:List[(Source,String)],identifier:String): SourcesCollector = {
    //create global index
    val namespacing =
      scalameta
        .util
        .namespaces
        .collector
        .SourcesCollector(sources).resultingMap

    val res = sources
      //iterate through all source files and collect umlUnits
      //then merge them together using the semigroup property
      //of SourceCollector
      .foldLeft(Option.empty[SourceCollector]){
        case (acc,(source,compUnit)) =>
          val sourceCol = SourceCollector(source,GlobalContext(namespacing),compUnit)
          acc
          .map(s => s |+| sourceCol)
          .orElse(Some(sourceCol))
      }
      //All entities that are not defined in the collection of sources
      //are inserted from the externalReferences field.
      .map{ finalS =>
        val externals = finalS.resultingContext.localCon.externalReferences
        val externalsToInclude = for {e <- externals} yield {
          val topLevelElement = finalS.umlUnit.toplevelElements
          Option.when(!topLevelElement.exists{
            case c: uml.Class => c.identifier.equals(e.name) && c.namespace.equals(e.namespace)
            case _ => false
          })(e)
        }
        finalS.copy(umlUnit = finalS.umlUnit.copy(toplevelElements = finalS.umlUnit.toplevelElements ++ externalsToInclude.flatten))
      }
    SourcesCollector(res.map(s => s.umlUnit).getOrElse(UMLUnit("",Nil)))
  }
}
