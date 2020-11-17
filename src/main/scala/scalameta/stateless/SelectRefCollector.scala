package scalameta.stateless

import scalameta.util.namespaces.NamespaceEntry

import scala.meta.Term

case class SelectRefCollector(namespaceAddition:NamespaceEntry)

object SelectRefCollector {
  def apply(ref:Term.Ref): SelectRefCollector = SelectRefCollector( ref match {
    case Term.Name(str) => NamespaceEntry(List(str))
    case Term.Select(Term.Name(n), name) => NamespaceEntry(List(n,name.value))
    case Term.Select(s:Term.Select,name) => this(s).namespaceAddition.append(name.value)
  })
}
