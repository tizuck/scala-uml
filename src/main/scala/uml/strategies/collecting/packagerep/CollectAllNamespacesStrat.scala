package uml.strategies.collecting.packagerep

import scalameta.util.namespaces.Entry
import uml.UMLElement
import uml.strategies.collecting.CollectStrategy

object CollectAllNamespacesStrat extends CollectStrategy[List[Entry]] {
  override def apply(v1: UMLElement, v2: List[Entry]): List[Entry] = v1 match {
    case c: uml.Class => (c.namespace :: v2).distinct
    case _ => v2
  }
}
