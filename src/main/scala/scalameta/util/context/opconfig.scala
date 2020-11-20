package scalameta.util.context

sealed case class OpEntry(op:String,rep:String)
sealed case class Ops(ops:List[OpEntry])
