package uml.strategies.collecting

import uml.UMLElement

trait CollectStrategy[T] extends ((UMLElement,T) => T)
