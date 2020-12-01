package uml.strategies.rewriting.packagerep

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.UMLUnit
import uml.strategies.rewriting.RewriteStrategy

object DeleteEmptyPackages extends RewriteStrategy[()] {
  override def apply(v1: ()): Strategy = {
    rulef {
      case u@UMLUnit(_,toplevelElements) =>
        val filtered = toplevelElements.filter( tp => tp match {
            case uml.Package(Nil,_,_)     => false
            case uml.Package(List(),_,_)  => false
            case _                        => true
          }
        )
        u.copy(toplevelElements = filtered)
      case u@_ => u
    }
  }
}
