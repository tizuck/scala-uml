package uml.strategies.rewriting.assoc

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{ClassRef, ConcreteClass, RelationshipElement, UMLUnit, externalReferences}
import uml.strategies.rewriting.RewriteStrategy

object DeleteUnTargetedExternalClasses extends RewriteStrategy[(List[RelationshipElement],List[RelationshipElement])]{
  override def apply(v1: (List[RelationshipElement], List[RelationshipElement])): Strategy = {
    val possiblyEliminated = v1._1
    val hitClasses = v1._2
    val difference = possiblyEliminated.diff(hitClasses)

    val f:Any => Any = {
      case u: UMLUnit =>
        u.copy(toplevelElements = u.toplevelElements.filterNot {
          case c: externalReferences.ClassDefRef =>
            checkMatchingClassRef(difference, c)
          case _ => false
        })
      case p: uml.Package => p
      case u@_ => u
    }
    rulef(f)
  }

  private def checkMatchingClassRef(difference: List[RelationshipElement],c:externalReferences.ClassDefRef): Boolean = {
    val res = difference.exists {
      case ClassRef(name, namespace) => name.equals(c.name) && namespace.equals(c.namespace)
      case ConcreteClass(cls) => cls.name.equals(c.name) && cls.namespace.equals(c.namespace)
    }
    res
  }
}
