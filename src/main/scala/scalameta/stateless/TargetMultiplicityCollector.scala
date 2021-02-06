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

package scalameta.stateless

import scala.meta.Type
import scala.meta.Type._

case class TargetMultiplicityCollector(multiplicity:String)

object TargetMultiplicityCollector {
  def apply(target : Type): TargetMultiplicityCollector = {
    target match {
      case Name(_) => new TargetMultiplicityCollector("1")
      case Apply(tpe, args) => tpe match {
        case Name(name) => name match {
          case "Option" => TargetMultiplicityCollector("[0..1]")
          case "List" => TargetMultiplicityCollector("*")
          case _ => TargetMultiplicityCollector("1")
        }
        case _ => TargetMultiplicityCollector("1")
      }
      case _ => TargetMultiplicityCollector("1")
    }
  }
}
