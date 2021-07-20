/*
 * Copyright 2021 Tilman Zuckmantel
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

package uml.strategies.rewriting

import app.ci.Filter
import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import uml.externalReferences.ClassDefRef
import uml._

object ExcludeStrategy extends RewriteStrategy[Filter]{
  override def apply(v1: Filter): Strategy = {
    val f: Any => Any = {
      case u: UMLUnit =>
        val excludedToplevelElements = u.toplevelElements
          .filterNot {
            case c: uml.Class =>
              matchesNameOrNamespace(v1, c.name, c.namespace.toString)
            case r: Relationship =>
              val fromB = r.relationshipInfo.from match {
                case ConcreteClass(cls) => matchesNameOrNamespace(v1, cls.name, cls.namespace.toString)
                case ClassRef(name, namespace) => matchesNameOrNamespace(v1, name, namespace.toString)
                case PackageRef(namespace) => matchesNameOrNamespace(v1, namespace.toString.dropRight(1), "")
              }
              val toB = r.relationshipInfo.to match {
                case ConcreteClass(cls) => matchesNameOrNamespace(v1, cls.name, cls.namespace.toString)
                case ClassRef(name, namespace) => matchesNameOrNamespace(v1, name, namespace.toString)
                case PackageRef(namespace) => matchesNameOrNamespace(v1, namespace.toString.dropRight(1), "")
              }
              fromB || toB
            case p: uml.Package => matchesNameOrNamespace(v1, p.namespace.toString.dropRight(1), "")
            case c: ClassDefRef => matchesNameOrNamespace(v1, c.name, c.namespace.toString + c.name)
            case _ => false
          }
        u.copy(toplevelElements = excludedToplevelElements)
      case p: uml.Package =>
        val excludedPackageBodyElements = p.packageBodyElements
          .filterNot {
            case c: uml.Class =>
              matchesNameOrNamespace(v1, c.name, c.namespace.toString)
            case r: Relationship =>
              val fromB = r.relationshipInfo.from match {
                case ConcreteClass(cls) => matchesNameOrNamespace(v1, cls.name, cls.namespace.toString)
                case ClassRef(name, namespace) => matchesNameOrNamespace(v1, name, namespace.toString)
                case PackageRef(namespace) => matchesNameOrNamespace(v1, namespace.toString.dropRight(1), "")
              }
              val toB = r.relationshipInfo.to match {
                case ConcreteClass(cls) => matchesNameOrNamespace(v1, cls.name, cls.namespace.toString)
                case ClassRef(name, namespace) => matchesNameOrNamespace(v1, name, namespace.toString)
                case PackageRef(namespace) => matchesNameOrNamespace(v1, namespace.toString.dropRight(1), "")
              }
              fromB || toB
            case p: uml.Package => matchesNameOrNamespace(v1, p.namespace.toString.dropRight(1), "")
            case _ => false
          }
        p.copy(packageBodyElements = excludedPackageBodyElements)
      case u@_ => u
    }
    rulef(f)
  }

  //Check if name without namespace matches regex
  //Check if name together with namespace matches regex
  private def matchesNameOrNamespace(f:Filter,name:String,namespace:String):Boolean = {
    val nameMatches =  f.matches(name)
    val namespaceMatches = f.matches(namespace+name)
    val res = nameMatches || namespaceMatches
    res
  }
}
