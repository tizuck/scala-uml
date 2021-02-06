package uml.strategies.rewriting.companion

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.strategies.rewriting.RewriteStrategy
import uml._

object InsertCompanionDependency extends RewriteStrategy[List[(uml.Class,Boolean)]] {
  override def apply(v1: List[(uml.Class, Boolean)]): Strategy = {
    val f: Any => Any = {
      case u: UMLUnit => v1.foldLeft(u) {
        case (acc, (c, b)) if b => acc.copy(toplevelElements = acc.toplevelElements.appended(
          Relationship(Annotation, Without, RelationshipInfo(None, None, ConcreteClass(c), ClassRef("$" + c.name, c.namespace), None, Without), List(Stereotype("companion", Nil)))
        ))
        case (acc, _) => acc
      }
      case o@_ => o
    }
    rulef(f)
  }
}
