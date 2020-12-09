package processors.repositories

sealed case class Entry(path:String)
sealed case class Directories(dirEntries:List[Entry])