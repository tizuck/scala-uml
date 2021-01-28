package uml.strategies.rewriting.packagerep

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.strategies.rewriting.RewriteStrategy
import uml._

object DeleteInnerAssocStrat extends RewriteStrategy[List[uml.Class]] {

  private def deleteRel[T <: UMLElement](elements: List[T], v1: List[uml.Class]): List[T] = {
    elements.filterNot {
      case Relationship(Inner, _, relationshipInfo, _) if relationshipInfo.originType.equals(externalReferences.Object) =>
        relationshipInfo.from match {
          case ConcreteClass(cls) =>
            v1.exists(c => c.name.equals(cls.name) && c.namespace.equals(cls.namespace))
          case ClassRef(name, namespace) =>
            v1.exists(c => c.name.equals(name) && c.namespace.equals(namespace))
        }
      case _ => false
    }
  }

  override def apply(v1: List[uml.Class]): Strategy = {
    rulef {
      case u@UMLUnit(_, toplevelElements) => u.copy(toplevelElements = deleteRel(toplevelElements, v1))
      case p@uml.Package(pkgElems, _, _) => p.copy(packageBodyElements = deleteRel(pkgElems, v1))
      case a@_ => a
    }
  }
}
