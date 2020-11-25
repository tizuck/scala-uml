package scalameta.stats.imports

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.UMLElement

import scala.meta.Importee
import scala.meta.Importee.Name

case class ImporteesCollector(names:List[String],fixme:String="fixme")

object ImporteesCollector {
  def apply(importees:List[Importee]): ImporteesCollector = {
    ImporteesCollector(importees.foldLeft(List.empty[String]){
      case (acc,n:Name) => acc ++ List(ImporteeNameCollector(n).name)
      case (acc,Importee.Wildcard()) => acc ++ List("")
      case (acc,_) => acc
    })
  }
}
