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

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ConcreteClass, Extension, Relationship, RelationshipInfo, Stereotype, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnRepeatedEnumCaseCollector(override val definedElements: List[UMLElement],
                                         override val resultingContext: CollectorContext) extends BaseCollector

object DefnRepeatedEnumCaseCollector {
  def apply(repeatedEnumCase:Defn.RepeatedEnumCase)(implicit context:CollectorContext):
  DefnRepeatedEnumCaseCollector = {
    val caseAsClasses = repeatedEnumCase.cases.foldLeft(List.empty[Class]){
      case (acc,enumCase) =>
       Class(isAbstract = false,enumCase.value,Nil,Nil,Nil,None,List(Stereotype("case",Nil))) :: acc
    }

    caseAsClasses.foldLeft(DefnRepeatedEnumCaseCollector(Nil,context)){
      case (acc,cls) =>
        val relationship =
          Relationship(
            Extension,
            ToFrom,
            RelationshipInfo(None,None,context.localCon.thisPointer.get,ConcreteClass(cls),None,Without)
            ,Nil
          )
        acc.copy(
          definedElements = cls :: relationship :: acc.definedElements,
        )
    }
  }
}
