package scalameta

import uml.{NamedElement, RelateableElement}


case class CollectorContext(thisPointer:Option[NamedElement with RelateableElement] = None,
                            definedTemplates:List[NamedElement with RelateableElement] = List.empty,
                            cstrOrigin:Option[String] = None) {

  def +(other:CollectorContext):CollectorContext =
    CollectorContext(
      this.thisPointer,
      (this.definedTemplates ++ other.definedTemplates).distinct,
      None)

  def ++(others:List[CollectorContext]):CollectorContext =
    others.foldLeft(this){
      case (acc,con) => acc + con
    }
}

