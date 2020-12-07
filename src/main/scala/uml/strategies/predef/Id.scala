package uml.strategies.predef

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.id
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

case class Id[T]() extends (T => Strategy) {
  override def apply(v1: T): Strategy = id
}
