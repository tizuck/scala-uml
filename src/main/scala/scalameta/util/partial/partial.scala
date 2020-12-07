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

package scalameta.util.partial

import scala.meta.{Defn, Tree}

object partial {
  def templates(target:String):PartialFunction[Tree,List[String]] = {
    case Defn.Trait(_,name,tparams,_,_) if name.value.equals(target) =>
      tparams.map(_.name.value)
    case Defn.Class(_,name,tparams,_,_) if name.value.equals(target) =>
      tparams.map(_.name.value)
    case Defn.Enum(_,name,tparams,_,_) if name.value.equals(target) =>
      tparams.map(_.name.value)
  }
}
