package uml.strategies.rewriting.packagerep

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scalameta.util.namespaces.Entry
import uml.UMLUnit
import uml.strategies.rewriting.RewriteStrategy

object InsertPackagesFromNamespacesStrat extends RewriteStrategy[List[Entry]] {
  override def apply(v1: List[Entry]): Strategy = {
    rulef {
      case u:UMLUnit=>
        val newPackages = v1.foldLeft(List.empty[uml.Package]){
          case (acc,entry) => acc ++ List(uml.Package(Nil,Nil,entry))
        }
        u.copy(toplevelElements = newPackages ++ u.toplevelElements)
      case u@_ => u
    }
  }
}
