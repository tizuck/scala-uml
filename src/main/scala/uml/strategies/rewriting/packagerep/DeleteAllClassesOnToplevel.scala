package uml.strategies.rewriting.packagerep

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scalameta.util.namespaces.DefaultNamespace
import uml.UMLUnit
import uml.strategies.rewriting.RewriteStrategy

object DeleteAllClassesOnToplevel extends RewriteStrategy[List[uml.Class]] {
  override def apply(v1: List[uml.Class]): Strategy = {
    rulef {
      case u@UMLUnit(_,toplevelElements) =>
        u.copy(toplevelElements = toplevelElements
          .diff(v1.filter(c => !c.namespace.equals(DefaultNamespace))))
      case u@_ => u
    }
  }
}
