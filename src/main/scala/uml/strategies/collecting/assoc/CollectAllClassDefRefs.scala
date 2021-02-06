package uml.strategies.collecting.assoc

import uml.UMLElement
import uml.externalReferences.ClassDefRef
import uml.strategies.collecting.CollectStrategy

object CollectAllClassDefRefs extends CollectStrategy[List[ClassDefRef]] {
  override def apply(v1: UMLElement, v2: List[ClassDefRef]): List[ClassDefRef] = v1 match {
    case c:ClassDefRef => v2.appended(c)
    case _ => v2
  }
}
