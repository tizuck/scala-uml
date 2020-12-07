package uml.strategies.collecting

import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.UMLElement

trait CollectStrategy[T] extends ((UMLElement,T) => T)
