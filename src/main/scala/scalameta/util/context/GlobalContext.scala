package scalameta.util.context

import uml.types.{DefinedTemplates, Namespace}

case class GlobalContext(globalScope:Map[Namespace,DefinedTemplates])
