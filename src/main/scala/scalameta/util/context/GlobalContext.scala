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

package scalameta.util.context

import scalameta.util.namespaces.{DefaultNamespace, Entry, Name, NamespaceEmpty, NamespaceEntry, Wildcard}

import scala.meta.{Defn, Stat}
import scalameta.util.namespaces

case class GlobalContext(globalScope:Map[Entry,List[(Stat,String)]]) {

  /**
   * inv: currentNamespace must match lastPkgNamespace after
   * multiple `currentNamespace.upperNamespace` iterations.
   *
   * @param nameOfStat
   * @param namespaceAddition
   * @param currentCompilationUnit
   * @param currentNamespace
   * @param lastPkgNamespace
   * @param imports
   * @return
   */
  def find(nameOfStat:String,
           namespaceAddition:Option[NamespaceEntry],
           currentCompilationUnit:String,
           currentNamespace:Entry,
           lastPkgNamespace:Entry,
           imports:Option[List[NamespaceEntry]])
    :Option[(Entry,Option[Stat])] = {

    def matchName(s:Stat): Boolean = s match {
      case t:Defn.Trait => t.name.value equals nameOfStat
      case c:Defn.Class => c.name.value equals nameOfStat
      case o:Defn.Object => o.name.value equals nameOfStat
      case e:Defn.Enum => e.name.value equals nameOfStat
      case _ => false
    }

    def sameCompUnit(namespace:Entry):Option[(Entry,Stat)] = {
      //Is there an entry in the global scope in this compilation unit for nameOfStat?
      val oEntry = globalScope.get(namespace)
      oEntry.flatMap(
        _.foldLeft(Option.empty[(Entry,Stat)]){
          case (acc@Some(_),_) => acc
          case (None,(stat,compUnit))  =>
            Option
              .when(compUnit.equals(currentCompilationUnit) && matchName(stat))(
                (namespace,stat)
              )
        }
      )
    }

    def diffCompUnit(namespace:Entry):Option[(Entry,Stat)] = {
      val oEntry = globalScope.get(namespace)
      oEntry.flatMap(
        _.foldLeft(Option.empty[(Entry,Stat)]){
          case (acc@Some(_),_) => acc
          case (None,(stat,compUnit))  =>
            Option
              .when(!compUnit.equals(currentCompilationUnit) && matchName(stat))(
                (namespace,stat)
              )
        }
      )
    }

    def importsName = {
      imports
        .map(_.filter(ne => ne match {
          case NamespaceEntry(_, Name) => true
          case _ => false
        }))
    }

    def matchingEntityInGlobalScope(oEntry: Option[List[(Stat, String)]],lookupNamespace:Entry) = {
      oEntry
        .flatMap(_.foldLeft(Option.empty[(Entry, Option[Stat])]) {
          case (acc@Some(_), _) => acc
          case (None, (stat, _)) => Option.when(matchName(stat))((lookupNamespace, Some(stat)))
        })
    }

    if(namespaceAddition.isDefined){
      //3 cases :
      //(i)     need to add the namespaceAddition to the current namespace and perform a lookup in current compilation unit
      //        -> Check in current namespace in current compilation unit
      //(ii)    need to add the namespaceAddition to each import and perform a lookup
      //(ii.1)  -> Check for explicit imports (import a.b.c  ...in code: c.d ...)
      //(ii.2)  -> Check for wildcard imports  (import a._   ...in code: b.c.d ...)
      //(iii)   need to respect namespaceAddition as a fully qualified namespace and perform a lookup
      //(iv)    need to add the namespaceAddition to the current namespace and perform a lookup in other compilation units

      val namespaceAdd = namespaceAddition.get.copy(targetType = namespaces.Package)
      //case (i)
      def inCurrentNamespace: Option[(Entry, Option[Stat])] = {
        val lookupNamespace = currentNamespace match {
          case DefaultNamespace => namespaceAdd.copy(targetType = namespaces.Package)
          case NamespaceEmpty => throw new IllegalStateException("unexpected empty Namespace")
          case NamespaceEntry(qualifiers, _) => NamespaceEntry(qualifiers ++ namespaceAdd.qualifiers,namespaces.Package)
        }
        sameCompUnit(lookupNamespace).map(tp => (tp._1,Some(tp._2)))
      }
      //case (ii.1)
      def inExplicitImport : Option[(Entry,Option[Stat])] = {
        importsName
          .flatMap {
            _.foldLeft(Option.empty[(Entry, Option[Stat])]) {
              case (acc@Some(_), _) => acc
              case (None, ne) =>
                val importTarget = ne.qualifiers.last
                val target = namespaceAdd.qualifiers.head
                //We found the matching import, however still need to find out if there is some stat in global scope
                Option.when(importTarget.equals(target)) {
                  val lookupNamespace = NamespaceEntry(ne.qualifiers ++ namespaceAdd.qualifiers.tail)
                  val oEntry = globalScope.get(lookupNamespace)
                  matchingEntityInGlobalScope(oEntry,lookupNamespace)
                    .orElse(Some((lookupNamespace,None)))
                }.flatten
            }
          }
      }
      //case (ii.2)
      def inWildcardImport : Option[(Entry,Option[Stat])] = {
        importsName
          .flatMap(_.foldLeft(Option.empty[(Entry,Option[Stat])]) {
            case (acc@Some(_), _) => acc
            case (None, ne) =>
              //add the namespace to the wildcard namespace and look if namespace exists
              val lookupNamespace = NamespaceEntry(ne.qualifiers ++ namespaceAdd.qualifiers)
              val oEntry = globalScope.get(lookupNamespace)
              oEntry
                .flatMap{e =>
                  val res = e.foldLeft(Option.empty[(Entry, Option[Stat])]) {
                    case (acc@Some(_), _) => acc
                    case (None, (stat, _)) => Option.when(matchName(stat))((lookupNamespace, Some(stat)))
                    }
                  res.orElse(Some((lookupNamespace,None)))
                }
          })
      }
      //case (iii)
      def absolute : Option[(Entry,Option[Stat])] = {
        val oEntry = globalScope.get(namespaceAdd)
        oEntry
          .flatMap(_.foldLeft(Option.empty[(Entry, Stat)]) {
            case (acc@Some(_), _) => acc
            case (None, (stat, _)) =>
              Option.when(matchName(stat))((namespaceAdd.copy(targetType = namespaces.Package), stat))
          })
          .map(tp => (tp._1, Some(tp._2)))
      }

      def inCurrentNamespaceDiffCompUnit : Option[(Entry,Option[Stat])] = {
        val lookupNamespace = currentNamespace match {
          case DefaultNamespace => namespaceAdd
          case NamespaceEmpty => throw new IllegalStateException("unexpected empty Namespace")
          case NamespaceEntry(qualifiers, _) => NamespaceEntry(qualifiers ++ namespaceAdd.qualifiers,namespaces.Package)
        }
        diffCompUnit(lookupNamespace).map(tp => (tp._1,Some(tp._2)))
      }

      inCurrentNamespace
        .orElse(inExplicitImport)
        .orElse(inWildcardImport)
        .orElse(absolute)
        .orElse(inCurrentNamespaceDiffCompUnit)
        //Treat namespace addition as the absolute namespace
        .orElse(Some(namespaceAdd,None))

    } else {

      def explicitImport : Option[(Entry,Option[Stat])] = {
         importsName
          //Of all namespaces: look in each namespace to find an explicit match
          .flatMap{_.foldLeft(Option.empty[(Entry, Option[Stat])]) {
                // if we already have found a matching entry then we are happy
                case (acc@Some(_), _) => acc
                // if there has not been a matching entry then proceed
                case (None, ne) =>
                  //determine the target of the current import
                  val target = ne.qualifiers.last
                  //only proceed further if the target name matches the looked up stat
                  Option.when(target.equals(nameOfStat)){
                    //Build namespace by eliminating the target from the qualifiers of the full namespace
                    val lookupNamespace = NamespaceEntry(ne.qualifiers.dropRight(1))
                    //Global lookup for the package
                    val oEntry = globalScope.get(lookupNamespace)
                    //in the entry of the globalScope for the package look if there is a matching entity
                    matchingEntityInGlobalScope(oEntry,lookupNamespace)
                      //In case we have a match of the targets, but target can't be found in the global scope
                      //we need to define a temp template in the namespace
                      .orElse(Some(lookupNamespace, None))

                    //If the target of the import does not match the searched entity proceed foldLeft with None
                  }.flatten
              }
          }
      }

      def wildcardImport : Option[(Entry,Stat)] = {
        imports
          .map(_.filter(ne => ne match {case NamespaceEntry(_,Wildcard) => true case _ => false}))
          .flatMap(_.foldLeft(Option.empty[(Entry,Stat)]){
            case (acc@Some(_),_) => acc
            case (None,ne) =>
              //package to look in is the full namespace with package target type
              val lookupNamespace = ne.copy(targetType = namespaces.Package)
              val oEntry = globalScope.get(lookupNamespace)
              oEntry
                .flatMap(_.foldLeft(Option.empty[(Entry,Stat)]){
                case (acc@Some(_),_) => acc
                case (None,(stat,_)) => Option.when(matchName(stat))((lookupNamespace,stat))
              })
          })
      }

    //(i) definition is in the same namespace and compilation unit as the current compilation unit
      // definition is in an upperNamespace in the same Compilation Unit
      val res = currentNamespace match {
        case DefaultNamespace => sameCompUnit(DefaultNamespace).map(tp => (tp._1,Some(tp._2)))
        case NamespaceEmpty => throw new IllegalStateException("Unexpected empty namespace.")
        case NamespaceEntry(qualifiers, _) =>
          def fullLookup(revQuals:List[String]):Option[(Entry,Option[Stat])] = revQuals match {
            case revQuals@_ :: _ =>
              sameCompUnit(NamespaceEntry(revQuals))
                .map(tp => (tp._1,Some(tp._2)))
                .orElse(fullLookup(revQuals.dropRight(1)))
            case Nil => sameCompUnit(DefaultNamespace).map(tp => (tp._1,Some(tp._2)))
          }
          fullLookup(qualifiers)
      }
      //(ii) definition is in an explicit import
      res
        .orElse(explicitImport)
      //(iii) definition is in a wildcard import
        .orElse(wildcardImport.map(tp => (tp._1,Some(tp._2))))
      //(iv) definition is in a different compilation unit of the same namespace
        .orElse(diffCompUnit(currentNamespace).map(tp => (tp._1,Some(tp._2))))
        //(v) definition is in a different compilation unit of an upper namespace
        .orElse(
          //if we are inside an object in a namespace
          if(!lastPkgNamespace.equals(currentNamespace)) {
            val upperNamespace = currentNamespace.upperNamespace
            diffCompUnit(upperNamespace).map(tp => (tp._1, Some(tp._2)))
          } else {
            None
          }
        )
      //(vi) definition is in a DefaultNamespace
        .orElse(diffCompUnit(DefaultNamespace).map(tp => (tp._1,Some(tp._2))))

    }
  }
}
