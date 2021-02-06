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
import scalameta.stats.StatsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, Stereotype, UMLElement}

import scala.meta.Defn

case class DefnEnumCollector(override val definedElements: List[UMLElement],
                             override val resultingContext: CollectorContext) extends BaseCollector

object DefnEnumCollector {
  def apply(defnEnum:Defn.Enum)(implicit context:CollectorContext): DefnEnumCollector = {
    val mods = ClassModsCollector(defnEnum.mods)
    val enumName = defnEnum.name

    val tempThisPointer = ClassRef(enumName.value,namespace = context.localCon.currentNamespace)
    val previousThisPointer = context.localCon.thisPointer
    val previousThisOrigin = context.localCon.thisOriginType

    val previousToplevel = context.localCon.isTopLevel
    val innerElements = StatsCollector(defnEnum.templ.stats)(
      context.withThisPointer(tempThisPointer).notToplevel.withThisOrigin(uml.externalReferences.Enum)
    )
    val innerOperations = innerElements.operations
    val innerAttributes = innerElements.attributes
    val primaryConstructor = PrimaryConstructorCollector(defnEnum.ctor)(
      innerElements
        .resultingContext
        .withCstrOrigin(enumName.value)
    )

    val cls = Class(
      isAbstract = true,
      enumName.value,
      innerAttributes,
      primaryConstructor.primaryCstr.map(List(_)).getOrElse(Nil) ++ innerOperations,
      mods.mods,
      None,
      List(Stereotype("scalaenum",Nil)),
      context.localCon.currentNamespace
    )

    DefnEnumCollector(
      cls :: innerElements.toplevel,
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
        .withThisOrigin(previousThisOrigin)
    )
  }
}
