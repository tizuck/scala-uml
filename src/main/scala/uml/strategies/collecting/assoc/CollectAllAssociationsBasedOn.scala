package uml.strategies.collecting.assoc

import uml.{Association, ClassRef, FromTo, Relationship, UMLElement}
import uml.externalReferences.ClassDefRef
import uml.strategies.collecting.CollectStrategy

object CollectAllAssociationsBasedOn extends (List[ClassDefRef] => CollectStrategy[List[Relationship]]) {
  override def apply(classDefRefs: List[ClassDefRef]): CollectStrategy[List[Relationship]] =
    (v1: UMLElement, v2: List[Relationship]) => v1 match {
      case r: Relationship if r.relationshipType.equals(Association) =>
        (r.relationshipInfo.from, r.relationshipInfo.to) match {
          case (c1: ClassRef, c2: ClassRef) =>
            if (r.relationshipDirection.equals(FromTo)) {
              if (classDefRefs.exists(c => c.name.equals(c2.name) && c.namespace.equals(c2.namespace))) {
                v2.appended(r)
              }
              else {
                v2
              }
            } else {
              if (classDefRefs.exists(c => c.name.equals(c1.name) && c.namespace.equals(c1.namespace))) {
                v2.appended(r)
              }
              else {
                v2
              }
            }
        }
      case _ => v2
    }
}
