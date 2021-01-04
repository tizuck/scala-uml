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

package scalameta.typeparams

import scalameta.util.context.CollectorContext
import uml.GenericParameter

import scala.meta.Type

case class TypeParamsCollector(typeParams:List[GenericParameter],contextBounds:Map[String,List[String]])

object TypeParamsCollector {
  def apply(tparams: List[Type.Param])(implicit context:CollectorContext): TypeParamsCollector = {
    tparams.foldLeft(new TypeParamsCollector(Nil,Map.empty)){
      case (acc,tparam) =>
        val typeParamCol = TypeParamCollector(tparam)
        val generic = typeParamCol.typeParam
        val cBounds = typeParamCol.contextBounds

        acc.copy(acc.typeParams ++ List(generic),contextBounds = acc.contextBounds + (generic.name -> cBounds ))
    }
  }
}