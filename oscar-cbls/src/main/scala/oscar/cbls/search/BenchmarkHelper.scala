package oscar.cbls.search

import oscar.cbls.objective.Objective
import oscar.cbls.search.core.Neighborhood

object Benchmark extends StopWatch{

  case class RunValues(it:Int,duration:Int,quality:Int)
  case class RunStatistics(it:Statistics,duration:Statistics,quality:Statistics){
    override def toString: String = "(it:" + it + " dur:" + duration + " obj:" + quality+")"
  }

  def benchToStatistics(obj:Objective, nRuns:Int, strategies:(()=>Neighborhood)*) =
    benchToTrace(obj, nRuns, strategies:_*).map{case (n:Neighborhood,t:IndexedSeq[RunValues]) => (n,aggregate(t.toList))}


  def benchToTrace(obj:Objective, nRuns:Int, strategies:(()=>Neighborhood)*)={
    val m = obj.model
    val initialSolution = m.solution()

    for(n <- strategies)
      yield (n(),for(trial <- 0 to nRuns) yield {
        m.restoreSolution(initialSolution)
        val strategyInstance = n()
        this.startWatch()
        val it = strategyInstance.doAllMoves(_ => false, obj)
        val time = this.getWatch
        val quality = obj.value
        RunValues(it,time.toInt,quality)
      })
  }

  def aggregate(l:List[RunValues]):RunStatistics = {
    RunStatistics(it=Statistics(l.map(_.it)),
      duration=Statistics(l.map(_.duration)),
      quality=Statistics(l.map(_.quality)))
  }
}

case class Statistics(min:Int, max:Int, avg:Int, med:Int){
  override def toString: String = "(min:" + min + " max:" + max + " avg:" + avg + " med:" + med + ")"
}

object Statistics {
  def apply(l: List[Int]): Statistics = {
    require(l.nonEmpty)
    val sorted = l.sorted
    val size = l.size
    Statistics(min=sorted.head, max = sorted.last, avg=l.sum/size, med=sorted.apply(size/2))
  }
}
