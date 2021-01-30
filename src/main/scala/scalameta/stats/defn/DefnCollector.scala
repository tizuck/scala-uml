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


import scalameta.stats.dcl.DclCollector
import scalameta.stats.defn.toplevel.{DefnDefToplevelCollector, DefnValToplevelCollector}
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.UMLElement

import scala.meta.{Decl, Defn, Type}

case class DefnCollector(definedElements:List[UMLElement],
                         override val resultingContext: CollectorContext) extends BaseCollector

object DefnCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): DefnCollector = {
    val defnRelationships = defn match {
      case v@Defn.Val(mods, pats, optionType , _) =>
        val dclRels = if(context.localCon.isTopLevel) {
          DefnValToplevelCollector(v)
        } else {
          DclCollector(
            Decl.Val(
              mods,pats,optionType.getOrElse(Type.Name("#notype#"))))(
            if(optionType.isEmpty){context.typeRequired} else context
          )
        }
        val ret = new BaseCollector  {
          override val definedElements: List[UMLElement] = dclRels.definedElements
          override val resultingContext: CollectorContext = dclRels.resultingContext
        }
        fromDecl(ret)
      case Defn.Var(mods,pats,optionType,_) =>
        val dclRels =
          DclCollector(Decl.Var(mods,pats,optionType.getOrElse(Type.Name("#notype#"))))(
            if(optionType.isEmpty){context.typeRequired} else context
        )
        fromDecl(dclRels.copy(resultingContext = dclRels.resultingContext))
      case d@Defn.Def(mods, name, tparams, paramss, optionType, _) =>
        val dclRels = if(context.localCon.isTopLevel){
          DefnDefToplevelCollector(d)(
            if(optionType.isEmpty){context.notTypeRequired} else context.typeRequired
          )
        } else {
          DclCollector(Decl.Def(mods,name,tparams,paramss , optionType.getOrElse(Type.Name("#notype#"))))(
            if(optionType.isEmpty){context.notTypeRequired} else context.typeRequired
          )
        }
        val ret = new BaseCollector  {
          override val definedElements: List[UMLElement] = dclRels.definedElements
          override val resultingContext: CollectorContext = context
        }
        fromDecl(ret)
      case t:Defn.Type => DefnTypeCollector(t)
      case t : Defn.Trait => TraitCollector(t)
      case c : Defn.Class => DefnClassCollector(c)
      case o : Defn.Object => DefnObjectCollector(o)
      case e : Defn.Enum => DefnEnumCollector(e)
      case re : Defn.RepeatedEnumCase => DefnRepeatedEnumCaseCollector(re)
      case ec : Defn.EnumCase => DefnEnumCaseCollector(ec)
    }

    new DefnCollector(
      defnRelationships.definedElements,
      defnRelationships.resultingContext
    )
  }

  private def fromDecl(dclRelationshipCollector: BaseCollector):BaseCollector = {
    new BaseCollector {
      override val definedElements: List[UMLElement] = dclRelationshipCollector.definedElements
      override val resultingContext: CollectorContext = dclRelationshipCollector.resultingContext
    }
  }
}
