package uml.strategies.rewriting

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.UMLUnit

object DistinctionStrat extends RewriteStrategy[Unit] {
  override def apply(v1: Unit): Strategy = {
    val f : Any => Any = u => u match {
      case u:UMLUnit => u.copy(toplevelElements = u.toplevelElements.distinct)
      case p:uml.Package => p.copy(packageBodyElements = p.packageBodyElements.distinct)
      case u@_ => u
    }
    rulef(f)
  }
}
