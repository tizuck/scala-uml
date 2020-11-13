package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty, NamespaceEntry}

import cats.implicits._

import scala.meta.Stat

case class StatsCollector(override val resultingMap: Map[Entry, List[Stat]]) 
  extends BaseNamespaceCollector 

object StatsCollector {
  def apply(stats:List[Stat],upperNamepace:Option[NamespaceEntry]): StatsCollector = {
    if(upperNamepace.isDefined) {
      StatsCollector(
        stats.foldLeft(Map[Entry, List[Stat]](
          upperNamepace.get -> List()
        )) {
          case (acc, stat) =>
            val statMap = StatCollector(stat).resultingMap
            statMap.foldLeft(acc) {
              case (innerAcc, NamespaceEmpty -> xs) =>
                innerAcc + (upperNamepace.get -> (xs ++ innerAcc(upperNamepace.get)).distinct)
              case (innerAcc, (n@NamespaceEntry(ns,_)) -> xs) =>
                if (innerAcc.contains(n.prepend(upperNamepace.get))) {
                  innerAcc + (n.prepend(upperNamepace.get) -> (xs ++ innerAcc(n.prepend(upperNamepace.get))).distinct)
                  //Tue aktuellen stat mit in den oberen Namespace hinein
                  innerAcc + (upperNamepace.get -> (stat :: innerAcc(upperNamepace.get)).distinct)
                } else {
                  innerAcc +
                    (n.prepend(upperNamepace.get) -> xs) +
                    (upperNamepace.get -> (stat :: innerAcc(upperNamepace.get)).distinct)
                }
            }
        })
    } else {
      StatsCollector(
        stats.foldLeft(Map.empty[Entry,List[Stat]]){
          case (acc,stat) =>
            val statNamespaces = StatCollector(stat)
            acc |+| statNamespaces.resultingMap
        })
      }
    }
  }
