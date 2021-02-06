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

import scalameta.stateless.TypeNameCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Class, Stereotype, UMLElement}

import scala.meta.{Defn, Pat}

case class DefnValToplevelCollector(override val definedElements: List[UMLElement],
                                    override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnValToplevelCollector {
  def apply(defnVal:Defn.Val)(implicit context:CollectorContext): DefnValToplevelCollector = {
    val classNames = defnVal.pats.foldLeft(List.empty[String]){
      case (acc,Pat.Var(name)) => acc ++ List(name.value)
    }

    val optionRetTypeRep =
      if(defnVal.decltpe.isDefined)
        Some(TypeNameCollector(defnVal.decltpe.get).typeRep)
      else
        None

    val clss = classNames.foldLeft(List.empty[Class]){
      case (acc,cn) =>
        acc ++ List(Class(
        isAbstract = false,
        cn.toUpperCase,
        List(Attribute(None,None,cn,optionRetTypeRep,Nil)),
          Nil,
          Nil,
          None,
          List(Stereotype("val",Nil))
      ))
    }

    DefnValToplevelCollector(clss,context)
  }
}
