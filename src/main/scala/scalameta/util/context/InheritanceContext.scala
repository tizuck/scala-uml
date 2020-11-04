package scalameta.util.context

import uml.{NamedElement, Operation, RelateableElement}

sealed case class InheritanceContext(toInherit:NamedElement with RelateableElement,
                                  cstr:Operation)
