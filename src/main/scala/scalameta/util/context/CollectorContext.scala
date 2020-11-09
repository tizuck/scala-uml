package scalameta.util.context

import scalameta.util.namespaces.NamespaceEntry
import uml.types.Namespace
import uml.{NamedElement, Operation, RelateableElement, RelationshipElement}

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

  def toplevel:CollectorContext = this.copy(localCon.copy(isTopLevel = true))
  def notToplevel : CollectorContext = this.copy(localCon.copy(isTopLevel = false))
  def withToplevel(isToplevel : Boolean) : CollectorContext = this.copy(localCon.copy(isTopLevel = isToplevel))
  def typeRequired : CollectorContext = this.copy(localCon.copy(typeRequired = true))
  def notTypeRequired : CollectorContext = this.copy(localCon.copy(typeRequired = false))
  def withNamespace(n:String): CollectorContext = this.copy(localCon.copy(currentNamespace = NamespaceEntry(List(n))))
  def witAdditionalImport(n:Namespace): CollectorContext = this.copy(localCon.copy(currentImports = localCon.currentImports.map(NamespaceEntry(List(n)) :: _)))
  def withCstrOrigin(cstrOrigin:String) = this.copy(localCon.copy(cstrOrigin = Some(cstrOrigin)))
  def withAdditionalTemplate(templ:NamedElement with RelateableElement) = {
    this.copy(localCon.copy(definedTemplates = localCon.definedTemplates ++ List(templ)))
  }
  def withThisPointer(r:RelationshipElement) = this.copy(localCon.copy(thisPointer = Some(r)))
  def withOptionalThisPointer(r:Option[RelationshipElement]) = this.copy(localCon.copy(thisPointer = r))
}

object CollectorContext {
  def apply(pre:GlobalContext): CollectorContext = {
    new CollectorContext(LocalContext(),pre)
  }
}
