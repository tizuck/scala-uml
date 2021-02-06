package uml.strategies.rewriting.assoc

import org.bitbucket.inkytonik.kiama.parsing.{ListParsers, NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.rewriting.PositionedRewriter.rulef
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}
import uml.{Attribute, ClassRef, ConcreteClass, FromTo, Relationship, RelationshipElement, ToFrom, UMLUnit}
import uml.strategies.rewriting.RewriteStrategy

object TransformAssociations extends RewriteStrategy[List[Relationship]]{
  override def apply(v1: List[Relationship]): Strategy = {
    val f: Any => Any = {
      case p: uml.Package => p.copy(packageBodyElements = p.packageBodyElements.map {
        case c: uml.Class => updateClass(v1, c)
        case u@_ => u
      })
      case u: UMLUnit => u.copy(toplevelElements = u.toplevelElements.map {
        case c: uml.Class => updateClass(v1, c)
        case u@_ => u
      })
      case u@_ => u
    }

    rulef(f)
  }

  private def updateClass(v1: List[Relationship], c: uml.Class) = {
    //Collect all relationships from list of all affected relationships v1
    //that originate from this class
    val allRelationshipsMatching = v1.foldLeft(List.empty[Relationship]) {
      case (acc, r) => r.relationshipDirection match {
        case FromTo => addIfClassMatchesRelationshipOrigin(acc, r.relationshipInfo.from, r, c)
        case ToFrom => addIfClassMatchesRelationshipOrigin(acc, r.relationshipInfo.to, r, c)
      }
    }

    val transformedAssociations = allRelationshipsMatching.foldLeft(List.empty[Attribute]) {
      case (acc, r) => r.relationshipDirection match {
        case FromTo => buildAttribute(acc, r, r.relationshipInfo.to)
        case ToFrom => buildAttribute(acc, r, r.relationshipInfo.from)
      }
    }
    c.copy(attributes = c.attributes ++ transformedAssociations)
  }

  private def buildAttribute(acc: List[Attribute], r: Relationship, rE:RelationshipElement) = {
    rE match {
      case c: ClassRef =>
        val typeName = c.name
        val interpreted = interpreteRelationshipId(r.relationshipInfo.relationshipIdentifier.getOrElse("noname"))
        val name = interpreted._1
        val generics = interpreted._2
        acc ++ List(Attribute(None, None, name, Some(typeName + generics), Nil, None))
      case c: ConcreteClass =>
        val typeName = c.cls.name
        val interpreted = interpreteRelationshipId(r.relationshipInfo.relationshipIdentifier.getOrElse("noname"))
        val name = interpreted._1
        val generics = interpreted._2
        acc ++ List(Attribute(None, None, name, Some(typeName + generics), Nil, None))
    }
  }

  private def addIfClassMatchesRelationshipOrigin(
                                                   acc: List[Relationship],
                                                   rE: RelationshipElement,
                                                   r:Relationship,
                                                   c: uml.Class):List[Relationship] = {

    rE match {
      case ClassRef(name, namespace) =>
        if (c.name.equals(name) && c.namespace.equals(namespace)) {
          acc ++ List(r)
        } else {
          acc
        }
      case ConcreteClass(cls) =>
        if (cls.name.equals(c.name) && cls.namespace.equals(c.namespace)) {
          acc ++ List(r)
        } else {
          acc
        }
    }
  }

  type TargetName = String
  type TargetGenerics = String

  private def interpreteRelationshipId(relId:String):(TargetName,TargetGenerics) = {
    case class TemplateParam(bind:String)
    case class Id(id:String)
    case class AssociationIdentifier(id:Id,templateParams: List[TemplateParam])

    val withoutSpaces = relId.replaceAll("\\s","")

    class Rules(positions:Positions) extends ListParsers(positions){
      lazy val assocId : Parser[AssociationIdentifier] = id ~ opt(temp) ^^ {
        tp => AssociationIdentifier(tp._1,tp._2.getOrElse(Nil))
      }

      lazy val temp : Parser[List[TemplateParam]] =
        "<<" ~> "bind" ~> rep1sep(id ~> "->" ~> typeRep,",") <~ ">>" ^^ {ids => ids.map(i => TemplateParam(i))}
      lazy val id : Parser[Id] = """[_a-zA-Z&|][_a-zA-Z0-9]*""".r ^^ Id
      //@todo this is hacky, maybe transfer this to a more clean approach later ;)
      //lazy val all : Parser[String] = regex("([^\\s(>>),(->)].)*".r)

      lazy val typeRep : Parser[String] = (
        id ~ gens ^^ {tp => tp._1.id + tp._2}
        | id ^^ {id => id.id}
      )

      lazy val gens : Parser[String] = "<" ~> rep1sep(typeRep,",") <~ ">" ^^ {s => "<"+s.mkString(",")+">"}
    }
    val parser = new Rules(new Positions)
    val parseRes = parser.parseAll(parser.assocId,StringSource(withoutSpaces))
    val assocId = parseRes match {
      case Success(result, _) => result
      case _:NoSuccess => AssociationIdentifier(Id(""),Nil)
    }

    val name = assocId.id.id
    val generics = if(assocId.templateParams.nonEmpty){
      assocId.templateParams.map(t => t.bind).mkString("<",",",">")
    } else {""}

    (name,generics)
  }
}
