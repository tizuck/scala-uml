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

package uml

import cats.Eval
import cats.data.State
import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEntry}
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import pretty.PrettyPrinter
import uml.externalReferences.ClassType

import scala.meta.Stat


sealed trait UMLElement { self =>

  type T <: UMLElement

  def pretty(implicit pretty:PrettyPrinter[T]) : String

  def structure : String
  def rewrite[T](s:T => Strategy)(startState:T)(f:(UMLElement,T) => T):Eval[(T,UMLElement)] = {
    val liftedS = State[T,Strategy](t => (t,s(t)))
    val liftedF: UMLElement => State[T,Unit] = u =>
      State(t => (f(u,t),()))
    rewrite(liftedS)(liftedF).run(startState)
  }

  def collect[B](pf:UMLElement ==> B):List[B] = {
    rewrite({(_:List[Option[B]]) => id})(List.empty[Option[B]])((u,t) => pf.lift(u) :: t ).value._1.flatten
  }

  def contains[B](elem:UMLElement):Boolean = {
    rewrite((_:Boolean) => id)(false)((u,t) => t || u.equals(elem)).value._1
  }

  def count(p:UMLElement => Boolean):Int = {
    rewrite((_:Int)=>id)(0)((u,amount) => if(p(u)) amount + 1 else amount).value._1
  }

  def map(f:UMLElement ==> UMLElement):UMLElement = {
    rewrite((_:Unit) => rule(f))(())((_, t) => t).value._2
  }

  def forall(p:UMLElement => Boolean):Boolean = {
    rewrite((_:Boolean) => id)(true)((u,t) => p(u) && t).value._1
  }

  def exists(p:UMLElement => Boolean):Boolean = {
    rewrite((_:Boolean) => id)(false)((u,t) => p(u) || t).value._1
  }

  def toList:List[UMLElement] = {
    rewrite((_:List[UMLElement]) => id)(Nil)((u,t) => t.appended(u)).value._1
  }

  protected[this] final def accStart[T,U <: UMLElement] : State[T,List[U]] = State(t => (t,Nil))
  protected[this] final def accInnerStart[T,U <: UMLElement] : State[T,List[List[U]]] = State(t => (t,Nil))

  /**
   * Builds a strategy to rewrite a term by a full traversal
   * over this instance of `UMLElement`.
   *
   * Allows to change the structure of a `UMLElement` while at
   * the same time update and consider a state of Type `T`.
   *
   * Strategy `s` that is wrapped inside a state monad is the core rewriting strategy
   * that defines how terms are rewritten. For example the following
   * strategy would define a term rewriting `Class.identifier` depending on
   * the current state:
   * {{{
   *   val s:State[String,Strategy] =
   *    State(newId =>
   *      rule {
   *        case c:uml.Class => c.copy(identifier=newId)
   *      }
   *    )
   * }}}
   *
   * f is used to update the state depending on the current instance.
   * The following function f for example updates a list of defined traits:
   * {{{
   *   val f:UMLElement => State[List[Class],Unit] = ue => ue match {
   *    case c:uml.Class if c.stereotype.equals(Stereotype("trait",Nil)) =>
   *      State(clss => (c :: clss,()))
   *   }
   * }}}
   *
   * This method can thus be used for multiple purposes. It can be used to simply collect
   * values when setting s to `State(_ => (_,id))` and defining the collection function in `f`.
   * It can be used to solely rewrite this instance by setting f to `(_ => State (_ => (_,())))`
   * or it can be used as a combination of both to create more advanced rewriting strategy.
   *
   * @param s rewriting strategy for the traversal of this ast.
   * @param f update state at each node of this ast in the traversal.
   * @tparam T Type of the state.
   * @return A Strategy that combines the functionalities of `s` and `f`.
   */
  def rewrite[T](s: State[T,Strategy])(f:UMLElement => State[T,Unit]):State[T,UMLElement]

  protected[this] final def rewriteList[T](s: State[T, Strategy], f:UMLElement => State[T,Unit], elements:List[UMLElement])
      : State[T, List[UMLElement]] = {
    elements
      .foldLeft(accStart[T,UMLElement]) { (acc, ue) =>
        for {
          currentList <- acc
          innerRewritten <- ue.rewrite(s)(f)
        } yield currentList.appended(innerRewritten)
      }
  }

  protected[this] final def rewriteListList[T](s: State[T, Strategy], f:UMLElement => State[T,Unit], elementss:List[List[UMLElement]])
  : State[T, List[List[UMLElement]]] = {
    elementss
      .foldLeft(accInnerStart[T,UMLElement]){
        case (acc,params) =>
          val res = params.foldLeft(accStart[T,UMLElement]){
            case (innerAcc,ue) =>for {
              currentList <- innerAcc
              rewrittenParam <- ue.rewrite(s)(f)
            } yield {
              currentList.appended(rewrittenParam)
            }
          }
          for {
            currentLists <- acc
            nextList <- res
          } yield {
            currentLists.appended(nextList)
          }
      }
  }

  protected[this] def rewriteOptionList[T](s: State[T, Strategy], f: UMLElement => State[T, Unit],option:Option[List[UMLElement]])
      : State[T, Option[List[UMLElement]]] = {

    if (option.isDefined) {
      val genParams = option.get
      for {
        gens <- rewriteList(s, f, genParams)
      } yield {
        Some(gens.asInstanceOf[List[GenericParameter]])
      }
    } else {
      State(t => (t,option))
    }
  }

  protected[this] def listStructure[T <: UMLElement](umlElements:List[T]):String =
    s"""List(${umlElements.map(_.structure).mkString(",")})"""

  protected[this] def optionString(os:Option[String]):String = {
    os match {
      case Some(value) => s"""Some("$value")"""
      case None => "None"
    }
  }

  protected[this] def optionUMLElement(oElement:Option[UMLElement]):String =  oElement match {
    case Some(value) => s"""Some(${value.structure})"""
    case None => "None"
  }

  protected[this] def optionAny[T](oA:T):String = oA match {
    case Some(value) => s"""Some(${value.toString})"""
    case None => "None"
  }
}

sealed trait StereotypeElement extends UMLElement {
  val stereotype: List[Stereotype]
}

sealed trait TopLevelElement extends UMLElement {
  val namespace : Entry = DefaultNamespace
}

sealed trait CompartmentElement extends UMLElement {

}

sealed trait PackageBodyElement extends UMLElement {

}

sealed trait NamedElement extends UMLElement {
  val name : String
}

sealed case class TaggedValue(name:String,value:Option[String]) extends NamedElement {

  override type T = TaggedValue

  override def structure: String = s"""TaggedValue("$name","${optionString(value)}")"""

  override def rewrite[T](s: State[T,Strategy])(f:UMLElement => State[T,Unit]):State[T,UMLElement] = {
    for {
      thisStrat <- s
      _ <- f(this)
    } yield Rewriter.rewrite(thisStrat)(this)
  }

  override def pretty(implicit pretty: PrettyPrinter[TaggedValue]): String = pretty.format(this).layout
}
sealed case class Stereotype(name:String,taggedValues:List[TaggedValue]) extends NamedElement {

  override type T = Stereotype

  override def structure: String = s"""Stereotype("$name",${listStructure(taggedValues)})"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      taggedRewritten <- rewriteList(s,f,taggedValues)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[Stereotype](
          {_.copy(taggedValues = taggedRewritten.asInstanceOf[List[TaggedValue]])}
        ) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Stereotype]): String = pretty.format(this).layout
}

sealed case class UMLUnit(name:String,
                          toplevelElements:List[TopLevelElement]) extends NamedElement {

  override type T = UMLUnit

  override def structure: String = s"""UMLUnit("$name",${listStructure(toplevelElements)})"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      rewrittenTopLevel <- rewriteList(s, f, toplevelElements)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[UMLUnit](u => u.copy(toplevelElements=rewrittenTopLevel.asInstanceOf[List[TopLevelElement]])) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[UMLUnit]): String = pretty.format(this).layout
}

/***************
 * Packages
 **************/

sealed case class Package(packageBodyElements:List[PackageBodyElement],
                          stereotype:List[Stereotype],
                          override val namespace:Entry=DefaultNamespace) extends
  TopLevelElement with
  PackageBodyElement with
  StereotypeElement {

  override type T = Package

  override def structure: String =
    s"""Package(${listStructure(packageBodyElements)},${listStructure(stereotype)}))"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      pkgBodyRewritten <- rewriteList(s, f, packageBodyElements)
      stereotypeRewritten <- rewriteList(s, f, stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(rule[Package](p =>
        p.copy(
          packageBodyElements = pkgBodyRewritten.asInstanceOf[List[Package]],
          stereotype = stereotypeRewritten.asInstanceOf[List[Stereotype]]
        )) <* thisStrat )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Package]): String = pretty.format(this).layout
}

/***************
 * Classes
 **************/

sealed case class GenericParameter(name:String,
                                   concreteType:Option[String],
                                   stereotype:List[Stereotype]) extends
  StereotypeElement with
  NamedElement {

  override type T = GenericParameter

  override def structure: String =
    s"""GenericParameter("$name",${optionString(concreteType)},${listStructure(stereotype)})"""


  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      stereotypeRewritten <- rewriteList(s,f,stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield  Rewriter.rewrite(rule[GenericParameter](g =>
      g.copy(stereotype = stereotypeRewritten.asInstanceOf[List[Stereotype]])
    ) <* thisStrat )(this)
  }

  override def pretty(implicit pretty: PrettyPrinter[GenericParameter]): String = pretty.format(this).layout
}

sealed trait AccessModifier
case object Private extends AccessModifier
case object Protected extends AccessModifier
case object PackagePrivate extends AccessModifier
case object Public extends AccessModifier

sealed trait Modificator
case object Static extends Modificator
case object Abstract extends Modificator

object externalReferences {

  sealed trait ClassType
  case object Trait extends ClassType
  case object Enum extends ClassType
  case object Object extends ClassType
  case object CClass extends ClassType
  case object CCaseClass extends ClassType

  sealed case class ClassDefRef(classtype:ClassType,
                                name:String, override val namespace:Entry,
                                templateParameter:List[String],
                                oStat:Option[Stat] = None)
    extends TopLevelElement with NamedElement {

    override type T = ClassDefRef

    override def structure: String =
      s"""ClassDefRef($classtype,"$name",$namespace,List(${templateParameter.mkString(",")}))"""

    override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
      for {
        _ <- f(this)
        thisStrat <- s
      } yield Rewriter.rewrite(thisStrat)(this)
    }

    override def pretty(implicit pretty: PrettyPrinter[ClassDefRef]): String = pretty.format(this).layout
  }


}


sealed case class Class(isAbstract:Boolean,
                        name:String,
                        attributes:List[Attribute],
                        operations:List[Operation],
                        additionalCompartements:List[Compartment],
                        genericParameters: Option[List[GenericParameter]],
                        stereotype : List[Stereotype],
                        override val namespace : Entry = DefaultNamespace) extends
  TopLevelElement with
  StereotypeElement with
  PackageBodyElement with
  NamedElement {

  override type T = Class

  override def structure: String =
    s"""Class($isAbstract,"$name",${
      listStructure(attributes)},${
      listStructure(operations)},${
      listStructure(additionalCompartements)},${
      if(genericParameters.map(listStructure).isDefined){
        "Some(" + genericParameters.map(listStructure).get + ")"
      } else {"None"}},${
      listStructure(stereotype)
    })""".stripMargin

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      attributesRewritten <- rewriteList(s,f,attributes)
      operationsRewritten <- rewriteList(s,f,operations)
      additionalCompartementsRewritten <- rewriteList(s,f,additionalCompartements)
      genericParametersRewritten <- rewriteOptionList(s, f, genericParameters)
      stereotypeRewritten <- rewriteList(s,f,stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[Class](c =>
          c.copy(
            attributes = attributesRewritten.asInstanceOf[List[Attribute]],
            operations = operationsRewritten.asInstanceOf[List[Operation]],
            additionalCompartements = additionalCompartementsRewritten.asInstanceOf[List[Compartment]],
            genericParameters = genericParametersRewritten.asInstanceOf[Option[List[GenericParameter]]],
            stereotype = stereotypeRewritten.asInstanceOf[List[Stereotype]]
          )
        ) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Class]): String = pretty.format(this).layout
}
/***************
 * Attributes
 **************/

sealed case class Attribute(modificators:Option[List[Modificator]],
                            modifier: Option[AccessModifier],
                            name:String,
                            attributeType:Option[String],
                            stereotype:List[Stereotype],
                            defaultValue:Option[String] = None) extends
  CompartmentElement with
  StereotypeElement with
  NamedElement {

  override type T = Attribute

  override def structure: String = s"""Attribute(${if(modificators.isDefined){
    s"""Some(${modificators.get.map(_.toString).mkString(",")})"""
  } else "None"},${
    optionAny(modifier)
  },"$name",${
    optionString(attributeType)},${listStructure(stereotype)})"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      stereotypeRew <- rewriteList(s,f,stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[Attribute]( a =>
          a.copy(
            stereotype = stereotypeRew.asInstanceOf[List[Stereotype]]
          )
        ) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Attribute]): String = pretty.format(this).layout
}

/***************
 * Operations
 **************/


sealed case class Parameter(name:String,
                            paramType:String,
                            stereotype:List[Stereotype]) extends
  StereotypeElement with
  NamedElement {

  override type T = Parameter

  override def structure: String = s"""Parameter("$name","$paramType",${listStructure(stereotype)})"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      stereotypeRewritten <- rewriteList(s, f, stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[Parameter](p =>
          p.copy(stereotype = stereotypeRewritten.asInstanceOf[List[Stereotype]])
        ) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Parameter]): String = pretty.format(this).layout
}


sealed case class Operation(modificator: Option[List[Modificator]],
                            accessModifier: Option[AccessModifier],
                            name:String,
                            paramSeq:List[List[Parameter]],
                            returnType:Option[String],
                            stereotype:List[Stereotype],
                            templateParameter:Option[List[GenericParameter]] = None) extends
  CompartmentElement  with
  StereotypeElement with
  NamedElement {
  override type T = Operation

  override def structure: String = s"""Operation(${
    modificator.map(m => s"""Some(List(${m.toString.mkString(",")}))""").getOrElse("None")
  },${optionAny(accessModifier)},"$name",${
    templateParameter.map(g => "Some(" + g.map(_.structure).mkString(",") + ")").getOrElse(None)
  },${
    if(paramSeq.isEmpty || paramSeq.head.isEmpty){"List(List())"} else {paramSeq.map(seq => s"""List(${seq.map(_.structure).mkString(",")})""")}
  },${optionString(returnType)},${listStructure(stereotype)})"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      rewrittenParameter <- rewriteListList(s,f,paramSeq)
      rewrittenStereotype <- rewriteList(s,f,stereotype)
      rewirrtenGenericParams <- rewriteOptionList(s,f,templateParameter)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(rule[Operation]( o =>
        o.copy(
          paramSeq = rewrittenParameter.asInstanceOf[List[List[Parameter]]],
          stereotype = rewrittenStereotype.asInstanceOf[List[Stereotype]],
          templateParameter = rewirrtenGenericParams.asInstanceOf[Option[List[GenericParameter]]]
        )
      ) <* thisStrat)(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Operation]): String = pretty.format(this).layout
}


sealed case class Compartment(identifier:Option[String],
                              taggedValues:List[TaggedValue],
                              stereotype:List[Stereotype]) extends
  UMLElement with
  StereotypeElement {

  override type T = Compartment
  override def structure: String =
    s"""Compartment("$identifier",${listStructure(taggedValues)},${listStructure(stereotype)}"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      rewTaggedVals <- rewriteList(s,f,taggedValues)
      rewStereotypes <- rewriteList(s,f,stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(rule[Compartment](c =>
        c.copy(
          taggedValues = rewTaggedVals.asInstanceOf[List[TaggedValue]],
          stereotype = rewStereotypes.asInstanceOf[List[Stereotype]])
      ) <* thisStrat)(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Compartment]): String = pretty.format(this).layout
}

/***************
 * Relationships
 **************/

sealed trait RelationshipType
case object Extension extends RelationshipType
case object Realization extends RelationshipType
case object Composition extends RelationshipType
case object Aggregation extends RelationshipType
case object Annotation extends RelationshipType
case object Association extends RelationshipType
case object Inner extends RelationshipType

sealed trait RelationshipDirection
case object FromTo extends RelationshipDirection
case object ToFrom extends RelationshipDirection
case object Without extends RelationshipDirection

sealed trait RelationshipElement extends UMLElement

sealed case class ConcreteClass(cls:Class) extends RelationshipElement {

  override type T = ConcreteClass

  override def structure: String = s"ConcreteClass(${cls.structure})"

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      rewCls <- cls.rewrite(s)(f)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[ConcreteClass](c =>
          c.copy(
            rewCls.asInstanceOf[Class]
          )
        ) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[ConcreteClass]): String = pretty.format(this).layout
}
sealed case class ClassRef(name:String, namespace:Entry=DefaultNamespace) extends RelationshipElement {

  override type T = ClassRef

  override def structure: String = s"ClassRef($name,${namespace.toString})"

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(thisStrat)(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[ClassRef]): String = pretty.format(this).layout
}

sealed case class PackageRef(namespace: NamespaceEntry) extends RelationshipElement {

  override type T = PackageRef

  override def structure: String =
    s"PackageRef(${namespace.toString})"

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(thisStrat)(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[PackageRef]): String = pretty.format(this).layout

  override val toString: String = namespace.toString.dropRight(1)
}

sealed case class RelationshipInfo(sourceMultiplicity:Option[String],
                                   targetMultiplicity:Option[String],
                                   from: RelationshipElement,
                                   to: RelationshipElement,
                                   relationshipIdentifier:Option[String],
                                   identifierDirection:RelationshipDirection,
                                   originType:ClassType=externalReferences.Trait) extends UMLElement {

  override type T = RelationshipInfo
  def structure : String =
    s"""RelationshipInfo(${optionString(sourceMultiplicity)},${optionString(targetMultiplicity)},${
      from.structure},${to.structure},${optionString(relationshipIdentifier)},${identifierDirection.toString},$originType)"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      fromRew <- from.rewrite(s)(f)
      toRew <- to.rewrite(s)(f)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(rule[RelationshipInfo](ri =>
        ri.copy(
          from = fromRew.asInstanceOf[RelationshipElement],
          to = toRew.asInstanceOf[RelationshipElement]
        )
      ) <* thisStrat)(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[RelationshipInfo]): String = pretty.format(this).layout
}

sealed case class Relationship(relationshipType: RelationshipType,
                               relationshipDirection: RelationshipDirection,
                               relationshipInfo: RelationshipInfo,
                               stereotype:List[Stereotype]) extends
  TopLevelElement with
  PackageBodyElement with
  StereotypeElement {

  override type T = Relationship

  override def structure: String =
    s"""Relationship(${relationshipType.toString},${relationshipDirection.toString},${
      relationshipInfo.structure},${listStructure(stereotype)})"""

  override def rewrite[T](s: State[T, Strategy])(f: UMLElement => State[T, Unit]): State[T, UMLElement] = {
    for {
      relationshipInfoRew <- relationshipInfo.rewrite(s)(f)
      stereotypesRew <- rewriteList(s,f,stereotype)
      _ <- f(this)
      thisStrat <- s
    } yield {
      Rewriter.rewrite(
        rule[Relationship]( r =>
          r.copy(
            relationshipInfo = relationshipInfoRew.asInstanceOf[RelationshipInfo],
            stereotype = stereotypesRew.asInstanceOf[List[Stereotype]]
          )
        ) <* thisStrat
      )(this)
    }
  }

  override def pretty(implicit pretty: PrettyPrinter[Relationship]): String = pretty.format(this).layout
}
