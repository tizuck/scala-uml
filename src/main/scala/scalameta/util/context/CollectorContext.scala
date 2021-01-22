/*
 * Copyright 2015 Tilman Zuckmantel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalameta.util.context

import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import uml.externalReferences.ClassDefRef
import uml.{NamedElement, Operation, RelationshipElement}

import scala.meta.Source

case class CollectorContext(localCon:LocalContext, globalCon:GlobalContext){

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
  def withNamespace(n:Entry): CollectorContext = this.copy(localCon.copy(currentNamespace = n))
  def withNamespace(n:String): CollectorContext = this.copy(localCon.copy(currentNamespace = NamespaceEntry(List(n))))
  def withAdditionalImports(ns:List[NamespaceEntry]) : CollectorContext =
    this.copy(
      localCon.copy(currentImports = if(localCon.currentImports.isDefined){
        Some(ns ++ localCon.currentImports.get)
      } else {
        Some(ns)
      }
      ))
  def withCstrOrigin(cstrOrigin:String): CollectorContext = this.copy(localCon.copy(cstrOrigin = Some(cstrOrigin)))
  def withThisPointer(r:RelationshipElement): CollectorContext = this.copy(localCon.copy(thisPointer = Some(r)))
  def withOptionalThisPointer(r:Option[RelationshipElement]): CollectorContext = this.copy(localCon.copy(thisPointer = r))
  def withExternalReference(e:ClassDefRef): CollectorContext = {
    if(localCon.externalReferences.contains(e)){
      this
    } else {
      this.copy(localCon.copy(externalReferences = this.localCon.externalReferences.appended(e)))
    }
  }
  def withLastPackageNamespace(pkgNamespace:NamespaceEntry):CollectorContext =
    this.copy(
      localCon = localCon.copy(lastPackageNamespace = pkgNamespace)
    )
  def withThisOrigin(classType:uml.externalReferences.ClassType) = this.copy(localCon.copy(thisOriginType = classType))

}

object CollectorContext {
  def apply(compilationUnit:String,pre:GlobalContext): CollectorContext = {
    new CollectorContext(LocalContext(compilationUnit),pre)
  }
}
