package uml.strategies.rewriting.assoc

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.{Relationship, UMLUnit}
import uml.strategies.rewriting.RewriteStrategy

object DeleteAssocs extends RewriteStrategy[List[Relationship]]{
  override def apply(v1: List[Relationship]): Strategy = {
    val f : Any => Any = {
      case u: UMLUnit =>
        u.copy(toplevelElements = u.toplevelElements.filterNot {
          case r: Relationship if v1.contains(r) => true
          case _ => false
        })
      case p: uml.Package =>
        p.copy(packageBodyElements = p.packageBodyElements.filterNot {
          case r: Relationship if v1.contains(r) => true
          case _ => false
        })
      case u@_ => u
    }

    rulef(f)
  }
}
