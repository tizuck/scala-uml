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

package scalameta.stats.defn

import scalameta.cstr.PrimaryConstructorCollector
import scalameta.mods.ClassModsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, Stereotype, UMLElement}

import scala.meta.Defn

case class DefnEnumCaseCollector(override val definedElements: List[UMLElement],
                                 override val resultingContext: CollectorContext)  extends BaseCollector

object DefnEnumCaseCollector {
  def apply(defnEnumCase:Defn.EnumCase)(implicit context:CollectorContext): DefnEnumCaseCollector = {
    val mods = ClassModsCollector(defnEnumCase.mods)
    val caseName = defnEnumCase.name.value

    val tempThisPointer = ClassRef(caseName,namespace = context.localCon.currentNamespace)
    val previousThisPointer = context.localCon.thisPointer
    val previousThisOrigin = context.localCon.thisOriginType

    val inheritedElements = InitsCollector(defnEnumCase.inits)(
      context.withThisPointer(tempThisPointer).withThisOrigin(uml.externalReferences.Enum)
    )
    val primaryConstructor = PrimaryConstructorCollector(defnEnumCase.ctor)(
      inheritedElements.resultingContext.withCstrOrigin(caseName)
    )

    val cls = Class(
      isAbstract = false,
      caseName,
      Nil,
      primaryConstructor.primaryCstr.map(List(_)).getOrElse(Nil),
      mods.mods,
      None,
      List(Stereotype("case",Nil)),
      context.localCon.currentNamespace
    )

    new DefnEnumCaseCollector(
      cls :: inheritedElements.definedElements,
      inheritedElements.resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withThisOrigin(previousThisOrigin)
    )
  }
}
