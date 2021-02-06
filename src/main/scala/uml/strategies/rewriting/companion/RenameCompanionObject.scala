package uml.strategies.rewriting.companion

import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.strategies.rewriting.RewriteStrategy

object RenameCompanionObject extends RewriteStrategy[List[(uml.Class,Boolean)]]{
  override def apply(v1: List[(uml.Class, Boolean)]): Strategy = {
    val f: Any => Any = {
      case c: uml.Class if c.stereotype.exists(s => s.name.equals("object")) =>
        //we found a match
        if (v1.exists(tp => tp._2 && tp._1.name.equals(c.name) && tp._1.namespace.equals(c.namespace))) {
          c.copy(name = "$" + c.name)
        } else {
          c
        }
      case u@_ => u
    }
    rulef(f)
  }
}
