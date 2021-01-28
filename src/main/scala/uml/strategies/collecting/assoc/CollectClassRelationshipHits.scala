package uml.strategies.collecting.assoc

import uml.{FromTo, Relationship, RelationshipElement, ToFrom, UMLElement, Without}
import uml.strategies.collecting.CollectStrategy

object CollectClassRelationshipHits extends CollectStrategy[List[uml.RelationshipElement]]{

  override def apply(v1: UMLElement, v2: List[RelationshipElement]): List[RelationshipElement] = v1 match {
    case r:Relationship =>
      val target = r.relationshipDirection match {
        case FromTo => r.relationshipInfo.to
        case ToFrom => r.relationshipInfo.from
          //@todo this is not quite correct
        case Without => r.relationshipInfo.to
      }
      if (v2.contains(target)) {
        v2
      }else {
        v2.appended(target)
      }
    case _ => v2
  }
}
