package scalameta.types

import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.{DefaultNamespace, NamespaceEntry}
import uml.{RefName, RefPathQualifier, RefTemplate}

import scala.meta.{Term, Type}

case class TargetTypeCollector(umlType:uml.Type)

object TargetTypeCollector {
  def apply(tpe:Type)(implicit context:CollectorContext):TargetTypeCollector = tpe match {
    case Type.Name(n) => nameType(context, n)
    case Type.Apply(tpe,args) => templateType(context, tpe, args)
    // The resolution of this type in UML is tricky, because it is not directly depictable.
    // This can happen if a class is known as a member of a class and is defining an inner
    // class. Therefore it is sufficient to refer to the name
    case t@Type.Select(qual,name) => pathQualifier(qual, name)
    case Type.ApplyInfix(value, name, value1) => templateType(context,name,List(value,value1))
    
  }

  private def pathQualifier(qual: Term.Ref, name: Type.Name) = {
    def createNamespace(qual: Term.Ref): RefPathQualifier = qual match {
      case Term.Name(str) => RefPathQualifier(Nil, str)
      case Term.Select(term: Term.Select, name) =>
        val innerRes = createNamespace(term)
        //Root Node
        if (innerRes.path.isEmpty) {
          RefPathQualifier(List(innerRes.target), name.value)
        } else {
          innerRes.copy(
            path = innerRes.path.appended(innerRes.target),
            target = name.value
          )
        }
    }

    TargetTypeCollector(createNamespace(Term.Select(qual, Term.Name(name.value))))
  }

  private def templateType(context:CollectorContext, tpe: Type, args: List[Type]) = {
    val preType = TargetTypeCollector(tpe)(context).umlType
    val templateTypes =
      args
        .map(t => TargetTypeCollector(t)(context))
        .map(_.umlType)
    TargetTypeCollector(RefTemplate(preType, templateTypes))
  }

  /**
   * Performs global lookup of type with name `n` in context `context`.
   *
   * @param context current context of the position in the ast construction phase.
   * @param n name of the type to be looked up.
   * @return RefName of name + namespace if type known.
   *         RefName of name + defaultnamespace if type unknown.
   */
  private def nameType(context: CollectorContext, n: String): TargetTypeCollector = {
    val lookup = context.globalCon.find(
      n,
      None,
      context.localCon.currentCompilationUnit,
      context.localCon.currentNamespace,
      context.localCon.lastPackageNamespace,
      context.localCon.currentImports
    )
    lookup match {
      case Some(ref) => TargetTypeCollector(RefName(n, ref._1))
      case None => TargetTypeCollector(RefName(n, DefaultNamespace))
    }
  }
}
