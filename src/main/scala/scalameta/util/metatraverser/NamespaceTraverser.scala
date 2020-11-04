package scalameta.util.metatraverser

import uml.types.{DefinedTemplates, Namespace}

import scala.meta.{Pkg, Term, Traverser, Tree}

class NamespaceTraverser extends Traverser {
  type NamespaceMap = scala.collection.mutable.Map[Namespace,DefinedTemplates]

  private var map:NamespaceMap = scala.collection.mutable.Map.empty[Namespace,DefinedTemplates]

  override def apply(tree: Tree): Unit = tree match {
    case Pkg(ref,stats) =>
      val pkgNamespace : String = qualName(ref)
  }

  def qualName(qual:Term.Ref): String = qual match {
    case Term.Name(str) => str
    case Term.Select(s:Term.Select,name) => s"${qualName(s)}.${name.value}}"
    case Term.Select(n1:Term.Name,n2) => s"${n1.value}.${n2.value}"
    case _ => throw new IllegalStateException(s"unexpected Package name:$qual")
  }
}
