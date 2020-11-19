package scalameta.util

import uml.externalReferences.ClassDefRef
import uml.{Attribute, Class, NamedElement, Operation, RelateableElement, Relationship, UMLElement}

trait BaseCollector extends StateChangingCollector {
  val definedElements:List[UMLElement]
  
  def operations: List[Operation] = definedElements.flatMap{
    case o : Operation => Some(o)
    case _ => None 
  }

  def templates : List[NamedElement with RelateableElement] = definedElements.flatMap{
    case c:Class => Some(c)
    case _ => None
  }

  def innerElements : List[UMLElement] = definedElements.flatMap{
    case c:Class => Some(c)
    case r:Relationship => Some(r)
    case _ => None
  }

  def classDefRefs : List[UMLElement] = definedElements.flatMap{
    case c:ClassDefRef => Some(c)
    case _ => None
  }

  def attributes : List[Attribute] = definedElements.flatMap{
    case a:Attribute => Some(a) case _ => None
  }
}
