package scalameta.util

import uml.UMLElement

trait BaseCollector extends StateChangingCollector {
  val definedElements:List[UMLElement]
}
