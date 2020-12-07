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

import pretty.config.PlantUMLConfig
import pretty.plantuml.GenericParameterPretty
import scalameta.stateless.TypeNameCollector
import scalameta.util.context.CollectorContext
import uml.{GenericParameter, Stereotype}

import scala.meta.Type

case class TypeParamCollector(typeParam:GenericParameter,contextBounds:List[String])

object TypeParamCollector{
  def apply(typeParam:Type.Param)(implicit context:CollectorContext): TypeParamCollector = {
    val tParamMods = TypeParamModCollector(typeParam.mods)
    val parameterName = typeParam.name.value
    val stereotype = {
      if (tParamMods.isContravariant) {
        List(Stereotype("-",Nil))
      } else if(tParamMods.isCovariant) {
        List(Stereotype("+",Nil))
      } else Nil
    }
    val cbounds = TypeParamContextBoundsCollector(typeParam.cbounds).contextBounds

    val metaBound:Type.Bounds = typeParam.tbounds
    val bounds = TypeBoundsCollector(metaBound).typeRep
    val tparams = TypeParamsCollector(typeParam.tparams).typeParams

    //@todo fix me
    implicit val pretty = GenericParameterPretty()(PlantUMLConfig())

    val tparamsStringRep = if(tparams.nonEmpty){ s"<${tparams.map(_.pretty).mkString(",")}>"} else ""

    new TypeParamCollector(GenericParameter(parameterName + tparamsStringRep,bounds,stereotype),cbounds)
  }
}
