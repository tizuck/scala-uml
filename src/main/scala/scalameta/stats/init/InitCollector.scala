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

package scalameta.stats.init


import scalameta.stateless.TargetTypeCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml._
import uml.externalReferences.{CClass, ClassDefRef, ClassType}

import scala.meta.{Case, Defn, Init}

case class InitCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext) extends BaseCollector

object InitCollector {
  def apply(init:Init)(implicit context:CollectorContext): InitCollector = {
    val extendedType = TargetTypeCollector(init.tpe)

    val classType:ClassType = extendedType.oTemplate.map {
      case _: Defn.Object => uml.externalReferences.Object
      case c: Defn.Class if c.mods.contains(Case) => uml.externalReferences.CCaseClass
      case _: Defn.Class => uml.externalReferences.CClass
      case _: Defn.Enum => uml.externalReferences.Enum
      case _: Defn.Trait => uml.externalReferences.Trait
      case _ => CClass
    }.getOrElse(CClass)

    val relationshipIdentifier =
      extendedType
        .boundTemplates
        .map{
          tbind => s"${tbind._1} -> ${tbind._2}"}
        .mkString(",")

    val mappedInitArgs = Option.when(init.argss.nonEmpty && init.argss.head.nonEmpty) {
      init
        .argss
        .flatten
        .map(_.syntax)
        .mkString(",")
        .prepended('[')
        .appended(']')
    }


    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(
        None,
        None,
        ClassRef(extendedType.target,extendedType.namespace),
        context.localCon.thisPointer.get,
        if(relationshipIdentifier.nonEmpty) Some(s"<<bind $relationshipIdentifier >>") else None,
        Without,
        originType = context.localCon.thisOriginType
      ),
      if(mappedInitArgs.nonEmpty)List(Stereotype("ctorBind",List(TaggedValue("vals",mappedInitArgs)))) else Nil
    )

    val classDefRef = ClassDefRef(
      classType,extendedType.target,
      extendedType.namespace,
      extendedType.boundTemplates.map(_._1),
      extendedType.oTemplate
    )
    new InitCollector(
      inheritance :: Nil,
      context.withExternalReference(classDefRef)
    )

  }
}
