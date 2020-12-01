package uml.strategies.collecting.packagerep

import uml.UMLElement
import uml.strategies.collecting.CollectStrategy

object CollectNamespaceObjectsStrat
  extends CollectStrategy[List[uml.Class]] {

  override def apply(v1: UMLElement, v2: List[uml.Class]): List[uml.Class] = v1 match {
    case c@uml.Class(_, _, _, _, _, _, str, _)
      if str.exists(_.name.equals("object")) ||
        str.exists(_.name.equals("caseobject")) =>
      v2.appended(c)
    case _ => v2
  }
}
