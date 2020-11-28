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

package scalameta.util.namespaces

import meta.{Ctor, Defn, Mod, Name, Pkg, Self, Source, Stat, Template, Term, Type}

object scalaDefaults {
  val default : Source =
    Source(
      List(
        Pkg(
          Term.Name("default"),
          List(
            Defn.Class(List(Mod.Sealed(), Mod.Abstract()), Type.Name("Option"), List(Type.Param(List(Mod.Covariant()), Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil)), Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), Nil)))
        )
      )
    )
}
