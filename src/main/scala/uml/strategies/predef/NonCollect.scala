package uml.strategies.predef

import uml.UMLElement
import uml.strategies.collecting.CollectStrategy

case class NonCollect[T]() extends CollectStrategy[T] {
  override def apply(v1: UMLElement, v2: T): T = v2
}
