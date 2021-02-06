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

package scalameta.stats.defn.toplevel


import scalameta.stats.dcl.DclCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, Operation, Stereotype, UMLElement}

import scala.meta.{Decl, Defn, Type}

case class DefnDefToplevelCollector(override val definedElements: List[UMLElement],
                                    override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnDefToplevelCollector {
  def apply(defnDef:Defn.Def)(implicit context:CollectorContext): DefnDefToplevelCollector = {
    val dclRels =
      DclCollector(
        Decl.Def(defnDef.mods,
          defnDef.name,
          defnDef.tparams,
          defnDef.paramss,
          defnDef.decltpe.getOrElse(Type.Name("#notype#"))))(
            if(defnDef.decltpe.isEmpty){context.copy(localCon = context.localCon.copy(typeRequired = false))}else context
      )
    val operation = dclRels.definedElements.flatMap{
      case o:Operation => Some(o)
      case _ => None
    }

    new DefnDefToplevelCollector(
      List(Class(
        isAbstract = false,
        defnDef.name.value.toUpperCase,
        Nil,
        operation,
        Nil,
        None,
        List(Stereotype("def",Nil))
      )),
      context
    )
  }
}
