package scalameta.stats.util

import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}

object defs {
  def obtainFurtherNamespace(old:Entry,add:String):NamespaceEntry = old match {
    case DefaultNamespace => NamespaceEntry(List(add))
    case NamespaceEntry(ns,tp) => NamespaceEntry(ns ++ List(add),tp)
  }
}
