package uml.strategies.rewriting
import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{ClassRef, ConcreteClass, FromTo, Inner, Relationship, UMLElement, UMLUnit}

object DeleteInnerAssocStrat extends RewriteStrategy[List[uml.Class]]{

  private def deleteRel[T <: UMLElement](elements: List[T], v1:List[uml.Class]) : List[T] = {
    elements.filterNot {
      case Relationship(Inner, _, relationshipInfo, _) =>
        relationshipInfo.from match {
          case ConcreteClass(cls) =>
            v1.exists(c => c.identifier.equals(cls.identifier) && c.namespace.equals(cls.namespace))
          case ClassRef(name, namespace) =>
            v1.exists(c => c.identifier.equals(name) && c.namespace.equals(namespace))
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
