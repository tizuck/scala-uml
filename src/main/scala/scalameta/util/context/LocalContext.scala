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

import java.io.IOException
import pureconfig.ConfigSource
import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import uml.RelationshipElement
import uml.externalReferences.{ClassDefRef, Trait}
import pureconfig._
import pureconfig.generic.auto._



case class LocalContext(thisPointer: Option[RelationshipElement] = None,
                        currentCompilationUnit : String,
                        cstrOrigin:Option[String] = None,
                        currentImports:Option[List[NamespaceEntry]] = None,
                        currentNamespace:Entry = DefaultNamespace,
                        typeRequired:Boolean = true,
                        isTopLevel:Boolean = true,
                        externalReferences:List[ClassDefRef] = Nil,
                        opReps:Ops,
                        lastPackageNamespace:Entry = DefaultNamespace,
                        thisOriginType:uml.externalReferences.ClassType = Trait)

object LocalContext {
  def apply(compilationUnit:String): LocalContext = {
    val loadedOpsConfig = ConfigSource.file("src/main/resources/operators.conf").load[Ops]
    val ops = loadedOpsConfig match {
      case Right(value) => value
      case l@_ =>  throw new IOException(s"src/main/resources/operators.conf is corrupt with output: $l.")
    }
    new LocalContext(
      currentCompilationUnit = compilationUnit,
      opReps = ops
    )
  }
}
