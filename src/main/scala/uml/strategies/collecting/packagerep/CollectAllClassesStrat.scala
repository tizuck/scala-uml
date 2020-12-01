package uml.strategies.collecting.packagerep

import uml.UMLElement
import uml.strategies.collecting.CollectStrategy

object CollectAllClassesStrat extends CollectStrategy[List[uml.Class]] {
  override def apply(v1: UMLElement, v2: List[uml.Class]): List[uml.Class] = v1 match {
    case c:uml.Class => c :: v2
    case _ => v2
  }
}
