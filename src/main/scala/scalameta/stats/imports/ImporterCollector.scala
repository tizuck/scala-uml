package scalameta.stats.imports

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.{Name, NamespaceEntry, Wildcard}
import uml.UMLElement

import scala.meta.Importer

case class ImporterCollector(namespaces:List[NamespaceEntry])

object ImporterCollector {
  def apply(importer:Importer): ImporterCollector = {
    val importees = ImporteesCollector(importer.importees).names
    val importNamespace = ImportRefCollector(importer.ref).namesspace
    ImporterCollector(importees.foldLeft(List.empty[NamespaceEntry]){
      case (acc,name) =>
        if(!name.isEmpty){
          acc ++ List(importNamespace.append(name).copy(targetType = Name))
        } else {
          acc ++ List(importNamespace.copy(targetType = Wildcard))
        }
    })
  }
}
