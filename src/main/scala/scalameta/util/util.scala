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

package scalameta.util

import scala.meta.Defn.{Class, Object, Trait, Type}
import scala.meta.{Pkg, Stat}

object util {
  def statToString(stat:Stat):String = stat match {
    case Pkg(ref, value) => s"""Pkg(${ref.structure})"""
    case Class(_,n,_,_,_) => s"Class($n)"
    case Trait(_,n,_,_,_) => s"Trait($n)"
    case Type(_,n,_,_) => s"Type($n)"
    case Object(value, name, template) => s"Object($name)"
  }
}
