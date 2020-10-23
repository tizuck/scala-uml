package scalameta

import scala.meta.{Source, Tree}

object astFunctionality {

  def foldCollect[T](t:Tree)(op:Tree => B)

  def foldLeft[B](t:Tree)(z:B)(op:Tree => B => B):B = t match {
    case s@Source(stats) => op(s)(foldList(stats)(z)(op))
  }

  private def foldList[B](trees:List[Tree])(z:B)(op:Tree => B => B):B =
    trees.foldLeft(z){(acc,tree) => foldLeft(tree)(acc)(op)}
}
