package scalameta.util.context

import scalameta.util.namespaces.{Entry, NamespaceEntry}
import uml.externalReferences.ClassDefRef
import uml.{NamedElement, Operation, RelateableElement, RelationshipElement}

import scala.meta.Source

case class CollectorContext(localCon:LocalContext, globalCon:GlobalContext) {

  def +(other: CollectorContext): CollectorContext =
    CollectorContext(
      localCon
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
  def withNamespace(n:Entry) = this.copy(localCon.copy(currentNamespace = n))
  def withNamespace(n:String): CollectorContext = this.copy(localCon.copy(currentNamespace = NamespaceEntry(List(n))))
  def withAdditionalImports(ns:List[NamespaceEntry]) : CollectorContext =
    this.copy(
      localCon.copy(currentImports = if(localCon.currentImports.isDefined){
        Some(ns ++ localCon.currentImports.get)
      } else {
        Some(ns)
      }
      ))
  def withCstrOrigin(cstrOrigin:String) = this.copy(localCon.copy(cstrOrigin = Some(cstrOrigin)))
  def withThisPointer(r:RelationshipElement) = this.copy(localCon.copy(thisPointer = Some(r)))
  def withOptionalThisPointer(r:Option[RelationshipElement]) = this.copy(localCon.copy(thisPointer = r))
  def withExternalReference(e:ClassDefRef) = {
    if(localCon.externalReferences.contains(e)){
      this
    } else {
      this.copy(localCon.copy(externalReferences = this.localCon.externalReferences.appended(e)))
    }
  }
}

object CollectorContext {
  def apply(compilationUnit:String,pre:GlobalContext): CollectorContext = {
    new CollectorContext(LocalContext(compilationUnit),pre)
  }
}
