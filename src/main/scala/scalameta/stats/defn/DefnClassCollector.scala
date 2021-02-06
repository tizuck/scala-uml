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


class DefnClassCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnClassCollector {
  def apply(defnClass:Defn.Class)(implicit context : CollectorContext): DefnClassCollector = {

    val mods = ClassModsCollector(defnClass.mods)
    val className = defnClass.name.value

    val typeParameters = TypeParamsCollector(defnClass.tparams).typeParams
    val genericParameter = Option.when(typeParameters.nonEmpty)(typeParameters)

    val tempThisPointer = ClassRef(className,namespace = context.localCon.currentNamespace)
    val previousThisPointer = context.localCon.thisPointer
    val previousThisOrigin = context.localCon.thisOriginType

    val inheritedElements = InitsCollector(defnClass.templ.inits)(
      context.withThisPointer(tempThisPointer).withThisOrigin(uml.externalReferences.CClass)
    )
    val previousToplevel = inheritedElements.resultingContext.localCon.isTopLevel
    val innerElements = StatsCollector(defnClass.templ.stats)(inheritedElements.resultingContext.notToplevel)
    val operations = innerElements.definedElements.flatMap{
      case o:Operation => Some(o)
      case _ => None
    }
    val primaryConstructor = PrimaryConstructorCollector(defnClass.ctor)(
      context.withCstrOrigin(className)
    )

    val cls = Class(
      mods.isAbstract,
      className,
      innerElements.attributes,
      primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++ operations,
      mods.mods,
      genericParameter,
      mods.classStereotypes,
      context.localCon.currentNamespace
    )

    val innerRelationship = if(previousThisPointer.isDefined){
      Some(
        Relationship(
          Inner,
          ToFrom,
          RelationshipInfo(
            None,
            None,
            previousThisPointer.get,
            ClassRef(className,context.localCon.currentNamespace),
            None,
            Without,
            originType = previousThisOrigin
          ),Nil
        )
      )
    } else None

    new DefnClassCollector(
      cls ::
        innerElements.innerElements ++
          inheritedElements.definedElements ++
          innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
        .withThisOrigin(previousThisOrigin)
    )
  }


}

