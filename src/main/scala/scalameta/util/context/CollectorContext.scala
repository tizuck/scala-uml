package scalameta.util.context

import uml.{NamedElement, Operation, RelateableElement}

import scala.meta.Source

case class CollectorContext(thisPointer: Option[NamedElement with RelateableElement] = None,
                            definedTemplates: List[NamedElement with RelateableElement] = List.empty,
                            cstrOrigin:Option[String]) {

  def +(other: CollectorContext): CollectorContext =
    CollectorContext(
      this.thisPointer,
      (this.definedTemplates ++ other.definedTemplates).distinct,
      this.cstrOrigin)

  def ++(others: List[CollectorContext]): CollectorContext =
    others.foldLeft(this) {
      case (acc, con) => acc + con
    }


}

object CollectorContext {
  def apply(): CollectorContext = {
    new CollectorContext(None,Nil,None)
  }
}
