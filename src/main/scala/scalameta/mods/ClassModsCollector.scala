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

package scalameta.mods

import scalameta.stateless.TypeNameCollector
import scalameta.util.context.CollectorContext
import uml.{Compartment, Stereotype, TaggedValue}

import scala.meta.Mod

case class ClassModsCollector(mods:List[Compartment], isAbstract:Boolean, classStereotypes:List[Stereotype] = Nil){
  self =>
  def appendScalaClass(other:TaggedValue):ClassModsCollector = {
    if(mods.exists(c => c.identifier.exists(s => s.equals("<<scalaclass>>")))){
      val updated = mods.map(c =>
        if(c.identifier.exists(s => s.equals("<<scalaclass>>"))){
          c.copy(taggedValues = c.taggedValues.appended(other))
        } else {
          c
        })
      self.copy(mods=updated)
    } else {
      val comp = Compartment(Some("<<scalaclass>>"),List(other),Nil)
      self.copy(mods = self.mods ++ List(comp))
    }
  }
  def appendAnnotation(annotation:String):ClassModsCollector = {
    val updated = if(mods.exists(c => c.identifier.exists(_.equals("<<annotated>>")))) {
      mods
        .map{c => (c,c.identifier.exists(_.equals("<<annotated>>")))}
        .map{tp => if(tp._2){
          //Annotations should always have a value on tagged values
          tp._1.copy(
            taggedValues = tp._1.taggedValues.map(t => t.copy(value = Some(s"${t.value.get.dropRight(1)},$annotation]")))
          )
        } else tp._1}
    } else {
      mods
        .appended(Compartment(Some("<<annotated>>"),List(TaggedValue("annotations",Some("[" + annotation + "]"))),Nil))
    }
    this.copy(mods=updated)
  }

  def appendClassStereotype(other:Stereotype):ClassModsCollector = {
    this.copy(classStereotypes = this.classStereotypes.appended(other))
  }

  def +(other:Boolean):ClassModsCollector = this.copy(isAbstract = other)

}

object ClassModsCollector {

  def apply(mods:List[Mod])(implicit context: CollectorContext): ClassModsCollector = {
    mods.foldLeft(ClassModsCollector(Nil,isAbstract = false)){
      case (acc,Mod.Final()) =>
        acc.appendScalaClass(TaggedValue("isFinal",None))
      case (acc,Mod.Private(ref)) =>
        acc.appendScalaClass(TaggedValue("privateIn",Some(s"${ref.syntax}")))
      case (acc,Mod.Override()) =>
        acc.appendScalaClass(TaggedValue("isOverride",None))
      case (acc,Mod.Open()) =>
        acc.appendScalaClass(TaggedValue("isOpen",None))
      case (acc,Mod.Abstract()) =>
        acc + true
      case (acc,Mod.Case()) =>
        acc.appendClassStereotype(Stereotype("caseclass",Nil))
      case (acc,Mod.Protected(ref)) =>
        acc.appendClassStereotype(Stereotype("protected",List(TaggedValue("in",Some(s"${ref.syntax}")))))
      case (acc,Mod.Opaque()) => acc.appendScalaClass(TaggedValue("isOpaque",None))
      case (acc,Mod.Lazy()) => acc.appendScalaClass(TaggedValue("isLazy",None))
      case (acc,Mod.Implicit()) => acc.appendScalaClass(TaggedValue("isImplicit",None))
      case (acc,Mod.Inline()) => acc.appendScalaClass(TaggedValue("isInline",None))
      case (acc,Mod.Sealed()) => acc.appendScalaClass(TaggedValue("isSealed",None))
      case (acc,Mod.Annot(a)) =>
        acc.appendAnnotation(
          "@".appendedAll(TypeNameCollector(a.tpe).typeRep)
            .appendedAll(Option.when(a.argss.nonEmpty && a.argss.head.nonEmpty)(
              a.argss.map(_.map(_.syntax).mkString(",")).mkString("(",")(",")")
            ).getOrElse(""))
        )
      case (acc,Mod.Transparent()) => acc.appendScalaClass(TaggedValue("isTransparent",None))
    }
  }
}


