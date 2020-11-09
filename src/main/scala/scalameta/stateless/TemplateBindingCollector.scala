package scalameta.stateless

import scala.meta.Type

case class TemplateBindingCollector(tBinding:Option[String])

object TemplateBindingCollector {
  def apply(mType:Type,templateParameter:List[String]): TemplateBindingCollector = mType match {
    case Type.Apply(tpe, args) => 
      //@todo we need a list of names of template parameter, for that we need a beforehand screening,
      //  that will collect all the information about the template parameter of a class.
      //  -> a before hand screening should not only collect the namespaces but also the elements in the namespace
      //  just a placeholder to compile
      TemplateBindingCollector(None)
  }
}
