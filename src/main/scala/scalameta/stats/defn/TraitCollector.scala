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
import scalameta.stats.init.InitsCollector
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml._

import scala.meta.Defn

case class TraitCollector(override val definedElements : List[UMLElement],
                          override val resultingContext: CollectorContext
                                     )
  extends BaseCollector

object TraitCollector {
  def apply(defnTrait:Defn.Trait)(implicit context:CollectorContext): TraitCollector = {

    val traitName = defnTrait.name.value
    val mods = ClassModsCollector(defnTrait.mods)
    val typeParameters = TypeParamsCollector(defnTrait.tparams).typeParams
    val genericParameter = Option.when(typeParameters.nonEmpty)(typeParameters)
    val tempThisPointer = ClassRef(traitName,namespace = context.localCon.currentNamespace)
    //Collect thisPointer for inner associations
    val previousThisPointer = context.localCon.thisPointer
    val previousOriginType = context.localCon.thisOriginType

    val inheritedElements = InitsCollector(defnTrait.templ.inits)(
      context.withThisPointer(tempThisPointer).withThisOrigin(uml.externalReferences.Trait)
    )
    val previousToplevel = inheritedElements.resultingContext.localCon.isTopLevel
    val innerElements = StatsCollector(defnTrait.templ.stats)(inheritedElements.resultingContext.notToplevel)

    if(defnTrait.name.value.equals("StereotypeElement")){
    }
    val primaryConstructor = PrimaryConstructorCollector(defnTrait.ctor)(
      context.withCstrOrigin(traitName)
    )
    val cls = Class(
      isAbstract = true,
      traitName,
      List.empty,
      primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++
        innerElements.operations,
      mods.mods,
      genericParameter,
      List(Stereotype("trait",Nil)),
      context.localCon.currentNamespace
    )

    val innerRelationship = previousThisPointer.flatMap( r =>
      Some(Relationship(
        Inner,
        ToFrom,
        RelationshipInfo(None,None,r,ClassRef(traitName,context.localCon.currentNamespace),
          None,
          Without,
          originType = previousOriginType),
        Nil)
      )
    )

    new TraitCollector(
      cls :: innerElements.innerElements ++ inheritedElements.definedElements ++ innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
        .withThisOrigin(previousOriginType)
    )
  }
}
