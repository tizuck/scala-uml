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

package scalameta.stats.util

import scalameta.stateless.{TargetMultiplicityCollector, TargetTypeCollector}
import scalameta.util.context.CollectorContext

import scala.meta.{Pat, Type}

trait AssociationInformation {
  val pDeclType : TargetTypeCollector
  val targetMultiplicity : String
  val pSources : List[String]
}

object AssociationInformation {
  def apply(pats:List[Pat],decltpe:Type)(implicit context: CollectorContext) : AssociationInformation =
    new AssociationInformation {
    override val pDeclType: TargetTypeCollector = TargetTypeCollector(decltpe)
    override val targetMultiplicity: String = TargetMultiplicityCollector(decltpe).multiplicity
    override val pSources: List[String] = pats.collect { _.syntax }
  }
}
