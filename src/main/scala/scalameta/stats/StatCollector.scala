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

package scalameta.stats

import org.slf4j.LoggerFactory
import scalameta.cstr.SecondaryConstructorCollector
import scalameta.stats.dcl.DclCollector
import scalameta.stats.defn.DefnCollector
import scalameta.stats.imports.ImportCollector
import scalameta.util.context.CollectorContext
import scalameta.util.{BaseCollector, StateChangingCollector}
import uml.UMLElement

import scala.meta.{Ctor, Decl, Defn, Import, Pkg, Stat}

case class StatCollector(definedElements : List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends StateChangingCollector

object StatCollector {
  def apply(stat:Stat)(implicit context: CollectorContext): StatCollector = {
    val relBase : BaseCollector = stat match {
      case pkg:Pkg => PkgCollector(pkg)
      case imprt:Import => ImportCollector(imprt)
      case decl: Decl => DclCollector(decl)
      case defn: Defn => DefnCollector(defn)
      case sctor: Ctor.Secondary =>
        val op = SecondaryConstructorCollector(sctor).ctor
        new BaseCollector {
          override val definedElements: List[UMLElement] = List(op)
          override val resultingContext: CollectorContext = context
        }
      case other@_ =>
        val logger = LoggerFactory.getLogger("uml-construction")
        logger.debug(s"found stat that is not supported: ${other.structure}")
        new BaseCollector {
        override val definedElements: List[UMLElement] = Nil
        override val resultingContext: CollectorContext = context
      }
    }

    new StatCollector(definedElements = relBase.definedElements,resultingContext = relBase.resultingContext)
  }
}
