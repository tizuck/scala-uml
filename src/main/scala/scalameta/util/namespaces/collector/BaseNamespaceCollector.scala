package scalameta.util.namespaces.collector

import scalameta.util.namespaces.{Entry, NamespaceEntry}

import scala.meta.{Defn, Pkg, Stat, Term}

trait BaseNamespaceCollector {

  val resultingMap : scala.collection.immutable.Map[Entry,List[Stat]]

}

object BaseNamespaceCollector {
  /**
   * builds the package name
   * @param qual
   * @return
   */
  def qualName(qual:Term.Ref): NamespaceEntry = qual match {
    case Term.Name(str) => NamespaceEntry(List(str))
    case Term.Select(s:Term.Select,Term.Name(str)) => qualName(s).append(str)
    case Term.Select(n1:Term.Name,n2:Term.Name) => NamespaceEntry(n1.value :: n2.value :: Nil)
    case _ => throw new IllegalStateException(s"unexpected Package name:$qual")
  }
}
