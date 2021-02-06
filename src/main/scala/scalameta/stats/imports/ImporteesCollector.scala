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

package scalameta.stats.imports

import scala.meta.Importee
import scala.meta.Importee.Name
import scala.reflect.ClassTag

case class ImporteesCollector(names:List[String])

object ImporteesCollector {
  def apply(importees:List[Importee])(implicit tag:ClassTag[Importee]): ImporteesCollector = {
    ImporteesCollector(importees.foldLeft(List.empty[String]){
      case (acc,n:Name) => acc ++ List(ImporteeNameCollector(n).name)
      case (acc,Importee.Wildcard()) => acc ++ List("")
      case (acc,_) => acc
    })
  }
}
