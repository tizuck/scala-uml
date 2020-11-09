package scalameta.util.context

import scalameta.util.namespaces.Entry
import uml.types.{DefinedTemplates, Namespace}

import scala.meta.Stat

case class GlobalContext(globalScope:Map[Entry,List[Stat]])
