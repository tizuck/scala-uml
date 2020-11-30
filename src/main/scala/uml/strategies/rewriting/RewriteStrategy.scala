package uml.strategies.rewriting

import org.bitbucket.inkytonik.kiama.rewriting.Strategy

trait RewriteStrategy[T] extends (T => Strategy)
