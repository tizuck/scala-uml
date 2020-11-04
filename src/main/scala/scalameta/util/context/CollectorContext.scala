package scalameta.util.context

import uml.{NamedElement, Operation, RelateableElement}

import scala.meta.Source

case class CollectorContext(localCon:LocalContext, globalCon:GlobalContext) {

  def +(other: CollectorContext): CollectorContext =
    CollectorContext(
      localCon.copy(definedTemplates = localCon.definedTemplates ++ other.localCon.definedTemplates)
      ,globalCon
    )

  def ++(others: List[CollectorContext]): CollectorContext =
    others.foldLeft(this) {
      case (acc, con) => acc + con
    }


}

object CollectorContext {
  def apply(pre:GlobalContext): CollectorContext = {
    new CollectorContext(LocalContext(),pre)
  }
}
