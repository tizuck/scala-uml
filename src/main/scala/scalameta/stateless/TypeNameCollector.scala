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

import org.slf4j.LoggerFactory
import scalameta.util.context.CollectorContext

import scala.meta.Type

case class TypeNameCollector(typeRep:String)

object TypeNameCollector {
  def apply(mtype:scala.meta.Type)(implicit context:CollectorContext): TypeNameCollector = {
    mtype match {
      case Type.Var(name) => new TypeNameCollector(name.syntax)
      case Type.Name(name) => new TypeNameCollector(name)
      case Type.Placeholder(bounds) =>
        val optBounds = BoundsNameCollector(bounds).bounds
        if(optBounds.isDefined)
          new TypeNameCollector(optBounds.get)
        else {
          new TypeNameCollector("WildcardType")
        }
      case Type.Apply(tpe,args) =>
        val applyType = TypeNameCollector(tpe).typeRep
        val argTypes = for (arg <- args) yield {
          TypeNameCollector(arg).typeRep
        }
        new TypeNameCollector(s"$applyType<${argTypes.mkString(",")}>")
      case Type.And(lhs,rhs) =>
        val lhsRep = this(lhs)
        val rhsRep = this(rhs)
        TypeNameCollector(s"&<${lhsRep.typeRep},${rhsRep.typeRep}>")
      case Type.Or(lhs, rhs) =>
        val lhsRep = this(lhs)
        val rhsRep = this(rhs)
        TypeNameCollector(s"|<${lhsRep.typeRep},${rhsRep.typeRep}>")
      case Type.With(lhs, rhs) =>
        TypeNameCollector(s"&<${this(lhs).typeRep},${this(rhs).typeRep}>")
      case Type.ApplyInfix(lhs,op,rhs) =>
        val lhsRep = this(lhs).typeRep
        val rhsRep = this(rhs).typeRep
        val opRep = context.localCon.opReps.ops.find(oe => oe.op.equals(op.value)).map(_.rep)
        TypeNameCollector(s"${opRep.getOrElse(op.value)}<$lhsRep,$rhsRep>")
      case Type.Function(params,res) =>
        val paramsReps = params.map(t => TypeNameCollector(t).typeRep)
        val paramsRep =
          if(params.size > 1) {
            s"Tuple${params.size}<${paramsReps.mkString(",")}>"
          } else {
            s"${paramsReps.head}"
          }

        val resRep = TypeNameCollector(res).typeRep
        TypeNameCollector(s"Func<$paramsRep,$resRep>")
        //@todo respect path dependent types here
      case Type.Select(qual,name) =>
        val resolvedQual = SelectRefCollector(qual).namespaceAddition
        TypeNameCollector(typeRep = resolvedQual.qualifiers.mkString("::").appendedAll(s"::$name"))

      case Type.Repeated(s) => TypeNameCollector(s"VarArgs<$s>")

      case Type.Tuple(args) =>
        TypeNameCollector(s"Tuple${args.size}<${
          args
            .map(arg => TypeNameCollector(arg).typeRep)
            .mkString(",")}>")
      case Type.ContextFunction(params,res) =>
        TypeNameCollector(
          s"ContextFunction${params.size}<${
            params
              .map(TypeNameCollector(_).typeRep)
              .mkString(",")
          },${TypeNameCollector(res).typeRep}>")
      case other =>
        LoggerFactory.getLogger("uml-construction")
          .debug(s"found type representation that is not yet supported: ${other.structure}")
        TypeNameCollector("")
    }
  }
}
