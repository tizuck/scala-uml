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

package scalameta.stats.dcl

import scalameta.parameters.ParamssCollector
import scalameta.stateless.{AccessModifierCollector, ModificatorsCollector, TypeNameCollector}
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Parameter, Stereotype, UMLElement}

import scala.meta.Decl

case class DclDefCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext) extends BaseCollector

object DclDefCollector {
  def apply(dclDef :Decl.Def)(implicit context:CollectorContext): DclDefCollector = {

    val operationName = dclDef.name.value
    //@todo get template parameter
    val parametersLists = ParamssCollector(dclDef.paramss).parameterLists

    val typeParamsCollector = TypeParamsCollector(dclDef.tparams)
    val typeParams = Option.when(typeParamsCollector.typeParams.nonEmpty)(typeParamsCollector.typeParams)
    val typeParamsCBounds = typeParamsCollector.contextBounds

    val returnType = if(context.localCon.typeRequired){Some(TypeNameCollector(dclDef.decltpe).typeRep)}else None

    val modificators = ModificatorsCollector(dclDef.mods).modificators

    val accessModifiers = AccessModifierCollector(dclDef.mods).accessModifier

    val interpretedCBounds = interpretCBounds(typeParamsCBounds)
    val op = uml.Operation(
      Option.when(modificators.nonEmpty)(modificators),
      accessModifiers,
      operationName,
      if(interpretedCBounds.isEmpty) parametersLists else parametersLists ++ List(interpretedCBounds),
      returnType,
      Nil,
      typeParams)

    new DclDefCollector(List(op),context)
  }

  private def interpretCBounds(cBounds:Map[String,List[String]]): List[Parameter] = {
    cBounds.foldLeft(List.empty[Parameter]){
      case (acc,(k,vs)) =>
        val cBounds = vs.map(v => Parameter("_",s"$v<$k>",List(Stereotype("using",Nil))))
        acc ++ cBounds
    }
  }
}
