package scalameta.stateless

import scala.meta.Type

case class TemplateBindingCollector(tBinding:Option[String])

object TemplateBindingCollector {
  def apply(mType:Type,templateParameter:List[String]): TemplateBindingCollector = mType match {
    case Type.Apply(tpe, args) => TemplateBindingCollector(None)
  }
}
