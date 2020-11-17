package scalameta.util.context

import scalameta.util.namespaces.{DefaultNamespace, Entry, Name, NamespaceEntry, Wildcard}

import scala.meta.{Defn, Stat}
import scalameta.util.namespaces

import scala.meta.Defn.Trait

case class GlobalContext(globalScope:Map[Entry,List[Stat]]) {
  /**
   * <p>Finds a stat in the global context with the according namespace-`Entry`. if namespace entry is
   * defined in imports but class is positioned in external project, then a namespace entry is returned
   * but no stat.</p>
   *
   * <p>If for example a reference to a template in code is `types.int.SmallInt` and the current
   * namespace is `DefaultNamespace` and imports contains an Entry `types::int -> List(...,Type("SmallInt"),...)`
   * then the call to this method is
   * `find("SmallInt", Some(NamespaceEntry(types,int)), DefaultNamespace, imports)`
   * with return `(NamespaceEntry(types,int),Some(Type("SmallInt")))`</p>
   *
   * @param nameOfStat stat name of stat to be found
   * @param namespaceAddition if a stat is referenced with additional namespace information like `a.b.C` this information
   *                          is passed here
   * @param currentNamespace namespace that has to be respected for relative import positioning
   * @param imports the list of imported namespaces with respect to the current position in code
   * @return `Entry` that fits the requirements with according stat or None if stat is not in project scope,
   *         None if no entry is found that matches the requirements
   */
  def find(nameOfStat:String,namespaceAddition:Option[NamespaceEntry],
           currentNamespace:Entry,
           imports:Option[List[NamespaceEntry]]):Option[(Entry,Option[Stat])] = {

    //3 cases :
    //(i) need to add the namespaceAddition to the current namespace and perform a lookup
    //(ii) need to add the namespaceAddition to each import and perform a lookup
    //(iii) need to respect namespaceAddition as a fully qualified namespace and perform a lookup
    if(namespaceAddition.isDefined){
      /**
       * Corresponds to case (i)
       */
      def relativeLookup(currentNamespace:NamespaceEntry,
                         namespaceAddition:NamespaceEntry):Option[(Entry,Option[Stat])] = {
        val absolutNamespace = currentNamespace.append(namespaceAddition)
        lookupStat(nameOfStat,absolutNamespace.copy(targetType = namespaces.Package))
          .map(tp => (tp._1,Some(tp._2)))
      }

      /**
       * Corresponds to case (ii)
       */
      def importLookup(imports:List[NamespaceEntry],
                       namespaceAddition:NamespaceEntry):Option[(Entry,Option[Stat])] = {
        imports.foldLeft(Option.empty[(Entry,Option[Stat])]){
          case (None,ne) => ne.targetType match {
            //target name must match the first qualifier in the namspaceAddition NamespaceEntry
            case Name | namespaces.Package =>
              val targetQual = ne.qualifiers.last
              val sourceQual = namespaceAddition.qualifiers.head
              if(!targetQual.equals(sourceQual)) {
                None
              } else {
                val lookupQual = ne.qualifiers ++ namespaceAddition.qualifiers.tail
                val lookup = lookupStat(nameOfStat,ne.copy(qualifiers = lookupQual,targetType = namespaces.Package))
                lookup.map(tp => (tp._1,Some(tp._2)))
                  .orElse(Some(NamespaceEntry(lookupQual),None))
              }
            //target can be anywhere in the referenced namespace
            case Wildcard =>
              val lookupQual =
                NamespaceEntry(ne.qualifiers ++ namespaceAddition.qualifiers,namespaces.Package)
              lookupStat(nameOfStat,lookupQual)
              .map(tp => (tp._1,Some(tp._2)))
              .orElse(
                if(namespaceExists(lookupQual)){
                  Some(lookupQual,None)
                }else {
                  None
                }
              )
          }
          case (acc,_) => acc
        }
      }

      /**
       * Corresponds to case (iii)
       */
      def absolutLookup(namespaceAddition:NamespaceEntry): Option[(Entry, Option[Stat])] = {
        lookupStat(nameOfStat,namespaceAddition.copy(targetType = namespaces.Package))
          .map(tp => (tp._1,Some(tp._2)))
          .orElse(Some(namespaceAddition,None))
      }

      currentNamespace match {
          //if default namespace then cases (ii) and (iii)
        case DefaultNamespace =>
          importLookup(imports.getOrElse(Nil),namespaceAddition.get)
            .orElse(absolutLookup(namespaceAddition.get))
          //if in some namespace in the project then cases (i) - (iii)
        case n:NamespaceEntry =>
          relativeLookup(n,namespaceAddition.get)
            .orElse(importLookup(imports.getOrElse(Nil),namespaceAddition.get))
            .orElse(absolutLookup(namespaceAddition.get))
        case _ => throw new IllegalArgumentException("unexpected empty Namespace")
      }
    } //Lookup perform without additional namespacing
      // 2 cases:
      // (a) Qualifier is defined in current namespace
      // (b) Qualifier is not defined in current namespace but in a namespace of an import
      // (c) Qualifier is defined in defaultNamespace
    else {

      def importLookup : Option[(Entry,Option[Stat])] = {
        if(imports.isDefined){
          imports.get.foldLeft(Option.empty[(Entry,Option[Stat])]){
            case (None,ne) => ne.targetType match {
              //template with name must be looked up in import namespace
              case Wildcard =>
                lookupStat(nameOfStat,ne.copy(targetType = namespaces.Package))
                  .map(tp => (tp._1,Some(tp._2)))
              case Name | namespaces.Package =>
                if(ne.qualifiers.last.equals(nameOfStat)) {
                  lookupStat(nameOfStat,ne.copy(qualifiers = ne.qualifiers.dropRight(1),targetType = namespaces.Package))
                    .map(tp => (tp._1,Some(tp._2)))
                    .orElse(Some(ne.copy(qualifiers = ne.qualifiers.dropRight(1),targetType = namespaces.Package),None))
                } else {
                  None
                }
            }
            case (acc,_) => acc
          }
        } else None
      }

      //(a)Qualifier is defined in current namespace
      lookupStat(nameOfStat,currentNamespace)
        .map(tp => (tp._1,Some(tp._2)))
        // (b) Qualifier is not defined in current namespace but in a namespace
        // of an import
        .orElse(importLookup)
        // (c) Qualifier is defined in Default namespace
        .orElse(lookupStat(nameOfStat,DefaultNamespace).map(tp => (tp._1,Some(tp._2))))
    }
  }

  private def namespaceExists(namespaceEntry: NamespaceEntry): Boolean ={
    globalScope.contains(namespaceEntry)
  }
  /**
   * Returns entry if global scope contains a template with name `statName`
   * in namespace nspace. None otherwise.
   *
   * Entries in the global scope are all of target type `namespaces.Package`.
   * Therefore only namepsaces with target type `namespaces.Package` are accepted.
   *
   * @param statName name of template to be looked up
   * @param nspace namespace for lookup, `targetType.equals(namespaces.Package)` is required
   */
  private def lookupStat(statName:String, nspace:Entry): Option[(Entry,Stat)] = {
    def lookup:Option[(Entry,Stat)] = {
      val oEntry = globalScope.get(nspace)
      oEntry match {
        case Some(stats) =>
          val stat = stats.find(stat =>
            stat match {
              case t:Trait => t.name.value equals statName
              case o:Defn.Object => o.name.value equals statName
              case e:Defn.Enum => e.name.value equals statName
              case c:Defn.Class => c.name.value equals statName
              case _ => false
            })
          if(stat.isDefined)
            Some(nspace,stat.get)
          else
            None
        case None =>None
      }
    }
    nspace match {
      case DefaultNamespace => lookup
      case n: NamespaceEntry if n.targetType.equals(namespaces.Package) => lookup
      case _ => throw new IllegalArgumentException(s"illegal namespace occurrence in $nspace")
    }
  }
}
