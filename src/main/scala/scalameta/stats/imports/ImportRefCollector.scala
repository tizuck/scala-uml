package scalameta.stats.imports

import scalameta.util.namespaces.NamespaceEntry

import scala.meta.Term

case class ImportRefCollector(namesspace:NamespaceEntry)

object ImportRefCollector {
  def apply(ref:Term.Ref): ImportRefCollector = ref match {
    case Term.Select(t:Term.Select,n:Term.Name) =>
      ImportRefCollector(ImportRefCollector(t).namesspace.append(n.value))
    case Term.Select(t1:Term.Name,t2:Term.Name) =>
      ImportRefCollector(NamespaceEntry(List(t1.value,t2.value)))
    case Term.Name(s) => ImportRefCollector(NamespaceEntry(List(s)))
  }
}
