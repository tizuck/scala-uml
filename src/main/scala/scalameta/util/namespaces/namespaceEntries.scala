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

package scalameta.util.namespaces

sealed trait Entry {
  self =>
  override def toString:String = self match {
    case DefaultNamespace => ""
    case NamespaceEmpty => ""
    case NamespaceEntry(qualifiers, targetType) =>qualifiers.mkString("::") + "."
  }
  def appended(other:Entry) : Entry
  def upperNamespace : Entry
}

case object NamespaceEmpty extends Entry {
  override def upperNamespace: Entry = this

  override def appended(other: Entry): Entry = other match {
    case NamespaceEmpty => NamespaceEmpty
    case DefaultNamespace => DefaultNamespace
    case n:NamespaceEntry => n
  }
}
case object DefaultNamespace extends Entry {
  override def upperNamespace: Entry = this

  override def appended(other: Entry): Entry = other match {
    case NamespaceEmpty => DefaultNamespace
    case DefaultNamespace => DefaultNamespace
    case n:NamespaceEntry => n
  }
}

sealed trait TargetType
case object Wildcard extends TargetType
case object Name extends TargetType
case object Package extends TargetType

/**
 * Entry of a namespace holding the qualifier in correct order in the
 * `qualifier` list.
 *
 * `NamespaceEntry(List("a","b","c"))` for example corresponds to
 * the namespace "a.b.c".
 *
 * @param qualifiers name of namespace qualifiers in correct order.
 */
sealed case class NamespaceEntry(qualifiers: List[String],targetType:TargetType = Package) extends Entry {

  def append(qualifier: String): NamespaceEntry =
    NamespaceEntry(qualifiers ++ List(qualifier))

  def append(namespaceEntry: NamespaceEntry): NamespaceEntry =
    NamespaceEntry(qualifiers ++ namespaceEntry.qualifiers)

  def prepend(namespaceEntry: NamespaceEntry): NamespaceEntry =
    NamespaceEntry(namespaceEntry.qualifiers ++ qualifiers)

  def prepend(qualifier: String): NamespaceEntry =
    NamespaceEntry(qualifier :: qualifiers)

  override def upperNamespace: Entry = {
    if(qualifiers.size > 1){
      this.copy(qualifiers = qualifiers.dropRight(1))
    } else {
      DefaultNamespace
    }
  }

  override def appended(other: Entry): Entry = other match {
    case NamespaceEmpty => this
    case DefaultNamespace => this
    case NamespaceEntry(qualifiers, targetType) => this.copy(qualifiers = this.qualifiers ++ qualifiers)
  }
}
