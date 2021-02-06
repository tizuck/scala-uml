package uml.strategies.rewriting.packagerep

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.strategies.rewriting.RewriteStrategy

object InsertClassesInPackageStrat extends RewriteStrategy[List[uml.Class]]{
  override def apply(v1: List[uml.Class]): Strategy = {
    rulef {
      case p@uml.Package(_, _,namespace) =>
        val classesMatch = v1.foldLeft(List.empty[uml.Class]){
          case(acc,cls) =>
            if(cls.namespace.equals(namespace)){
              acc ++ List(cls)
            }else {
              acc
            }
        }
        p.copy(packageBodyElements = p.packageBodyElements ++ classesMatch)
      case u@_ => u
    }
  }
}
