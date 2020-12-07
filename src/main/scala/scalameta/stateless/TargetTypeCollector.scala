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


import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.{DefaultNamespace, Entry}
import scalameta.util.partial.partial

import scala.meta.{Stat, Type}

case class TargetTypeCollector(namespace:Entry, target:String, boundTemplates:List[(String,String)], oTemplate:Option[Stat] )

object TargetTypeCollector {
  def apply(tpe:Type)(implicit context:CollectorContext): TargetTypeCollector = tpe match {
    case Type.Name(name) =>
      //Find name respecting current imports and target name
      val lookup = context
        .globalCon
        .find(
          name,
          None,
          context.localCon.currentCompilationUnit,
          context.localCon.currentNamespace,
          context.localCon.lastPackageNamespace,
          context.localCon.currentImports)

      lookup match {
        case Some(oEntry) => oEntry._2 match {
          case Some(stat) => TargetTypeCollector(oEntry._1,name,Nil,Some(stat))
          case None => TargetTypeCollector(oEntry._1,name,Nil,None)
        }
        case None => TargetTypeCollector(context.localCon.currentNamespace,name,Nil,None)
      }
      //For binding in UML no information about the inner template binding is needed, since
      //an inner template binding is just expressed directly in string form.
      //However, outer template binding is expressed as <<bind T_1 -> args[0],T_2 -> args[1]>>
    case Type.Apply(innerTpe,args) =>
      val innerTypeCol = ApplyTypeCollector(innerTpe)

      val target = innerTypeCol.target
      val namespace = innerTypeCol.namespace
      val oStat = innerTypeCol.oStat

      oStat match {
        case Some(stat) =>
          //A template that contains the template parameter has been found.
          val templates = stat.collect(partial.templates(target)).flatten
          val typeRepArgs = args.map(TypeNameCollector(_))
          //templates size and typeRepArgs size must have the same size
          //otherwise it would be illegal scala code
          if(templates.size != typeRepArgs.size){
            throw new IllegalStateException(s"Unexpected template binding in $templates and $typeRepArgs.")
          } else {
            val templateBindings = templates.foldLeft((typeRepArgs,List.empty[(String,String)])){
              case ((head :: tail,acc),templParam) =>
                (tail,acc ++ List((templParam,head.typeRep)))
              case ((Nil,acc),_) => (Nil,acc)
            }._2
             TargetTypeCollector(namespace,target,templateBindings,oStat)
          }
        case None =>
            val typeRepArgs = args.map(TypeNameCollector(_))
            val templateBindings = typeRepArgs.foldLeft((0,List.empty[(String,String)])){
              case ((i,acc),tpeCol) => (i+1,acc ++ List((s"T$i",tpeCol.typeRep)))
            }._2
            TargetTypeCollector(namespace,target,templateBindings,oStat)
        }

    case Type.Select(qual,name) =>

      val qualResolved = SelectRefCollector(qual)
      val lookup = context
      .globalCon
      .find(
        name.value,
        Some(qualResolved.namespaceAddition),
        context.localCon.currentCompilationUnit,
        context.localCon.currentNamespace,
        context.localCon.lastPackageNamespace,
        context.localCon.currentImports
      )

      new TargetTypeCollector(
        lookup.map(_._1).getOrElse(context.localCon.currentNamespace),
        name.value,
        Nil,lookup.flatMap(_._2)
      )

    case Type.Function(source, target) =>
      val lookup = context
      .globalCon
      .find(
        "Function",
        None,
        context.localCon.currentCompilationUnit,
        context.localCon.currentNamespace,
        context.localCon.lastPackageNamespace,
        context.localCon.currentImports
      )
      //@todo hack, should be corrected as soon as actual FunctionN is known due to URL import of scala standard library
      val functionNumber = source.size
      new TargetTypeCollector(
        DefaultNamespace,
        s"Function$functionNumber",
        source
          .zipWithIndex
          .map(tp => (s"T${tp._2}",TypeNameCollector(tp._1).typeRep))
          .appended(("R",TypeNameCollector(target).typeRep)),
        lookup.flatMap(_._2)
      )

  }
}
