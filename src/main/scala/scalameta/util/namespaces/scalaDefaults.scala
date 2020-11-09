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
