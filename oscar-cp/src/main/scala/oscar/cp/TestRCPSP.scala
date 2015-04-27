package oscar.cp

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.parallel.mutable.ParHashMap
import scala.io.Source
import oscar.algo.search.SearchStatistics
import oscar.cp.searches._
import oscar.nogood.database.NogoodDB
import oscar.nogood.decisions.Decision
import oscar.nogood.searches.BinaryConflictSet
import oscar.nogood.searches.FailureDirectedSearch
import oscar.nogood.searches.NogoodSearch
import oscar.nogood.searches.SplitConflictSet
import oscar.nogood.searches.RestartConflictSet
import oscar.nogood.searches.QuickShaving
import oscar.algo.search.DFSearch
import oscar.nogood.searches.BadDecisionSearch

object TestRCPSP extends App {
  
  case class Result(id: Int, instance: String, time: Long, nNodes: Int, nFails: Int, objective: Int, completed: Boolean) {
    override def toString: String = s"$instance\t$time\t$nFails\t$objective\t$completed\t${nNodes*1000.0/time.toDouble}"
  }

  val jType = "j120"
  val boundsFile = s"data/rcpsp/$jType/opt"
  val (lowerBounds, upperBounds) = BLParser.parseBounds(boundsFile)
  
  //val instances = Array((5, 2), (5, 7), (5, 10), (14, 3), (14, 10), (30, 10), (37, 2), (37, 6), (37, 7), (41, 1), (41, 2), (41, 7), (41, 8), (41, 9), (42, 3), (46, 6), (46, 7))
  
  val instances = for (i <- 1 to 60; j <- 1 to 10) yield (i, j)
  val filtered = instances.filter(i => {
    val instance = s"${jType.toUpperCase}_${i._1}_${i._2}.rcp"
    val ub = upperBounds(instance)
    val lb = lowerBounds(instance)
    lb < ub
  })
  
  val results = ParHashMap[String, Result]()
  
  println("name\ttime\tfails\tbest\topt\tnodes/sec")

  for ((j, i) <- instances.par) {

    new CPModel {
      solver.silent = true
      val instanceFile = s"${jType}_${j}_${i}"
      val instance = BLParser.parse(s"data/rcpsp/$jType/$instanceFile.rcp")

      // Data
      val nTasks = instance.nTasks
      val Tasks = 0 until nTasks
      val nResources = instance.nResources
      val Resources = 0 until nResources

      val scale = 1
      val durations = instance.durations.map(_ * scale)
      val demands = instance.demands.transpose
      val horizon = durations.sum
      val capa = instance.capacities

      // Decision variables
      val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t), s"start_$t"))
      val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
      val makespan = maximum(ends)

      // Precedences
      for ((t1, t2) <- instance.precedences) add(ends(t1) <= starts(t2))

      // Resources
      for (r <- Resources) {
        add(maxCumulativeResource(starts, durations.map(CPIntVar(_)), ends, demands(r).map(CPIntVar(_)), CPIntVar(capa(r))), Weak)
      }

      minimize(makespan)

      val nogoodDB = NogoodDB()
      val cpSearch = new NogoodSearch(solver, nogoodDB)
      val branching = new RestartConflictSet(starts, starts(_).min, starts(_).min)
      
      search (setTimes(starts, durations.map(CPIntVar(_)), ends, i => i))
      
      onSolution {     
        best = makespan.value
        solution = true
        t0 = System.currentTimeMillis()
      }
      
      val trueT0 = System.currentTimeMillis()
      var t0 = trueT0
      var n = 100
      var completed = solver.isFailed()
      var timeOut = false
      var solution = false
      var best = makespan.max
      
      cpSearch.onSolution {
        best = makespan.value
        solution = true
        //t0 = System.currentTimeMillis()
      }

      val timeLimit = 5000
      var nFails = 0
      var nNodes = 0
      var time = 0l
      
      /*val stats = solver.start(s => System.currentTimeMillis() - t0 >= timeLimit)
      nFails = stats.nFails
      nNodes = stats.nNodes
      time = System.currentTimeMillis() - t0
      completed = stats.completed
      timeOut = time > timeLimit*/
      
      

      while (!completed && !timeOut) {
        solver.pushState()
        nogoodDB.foreach(n => solver.post(n.toConstraint))
        cpSearch.start(branching, s => s.nBacktracks >= n || System.currentTimeMillis() - t0 >= timeLimit)
        solver.pop()
        
        nFails += cpSearch.nBacktracks
        nNodes += cpSearch.nNodes
        time = System.currentTimeMillis() - t0
        completed = cpSearch.isCompleted
        timeOut = time > timeLimit
        n = (n * 115) / 100
      }
   
      val result = Result(j*10 + i, instanceFile, System.currentTimeMillis() - trueT0, nNodes, nFails, best, completed)
      results += (instanceFile -> result)
      //println(result)
    }
  }
  
  println("name\ttime\tfails\tbest\topt\tnodes/sec")
  val allResults = results.values.toArray.sortBy(_.id) 
  val completed = allResults.count(r => r.completed)
  val sumObjs = allResults.map(_.objective).sum
  println(allResults.mkString("\n"))
  
  println("completed   : " + completed)
  println("uncompleted : " + (600 - completed))
  println("sumObjs     : " + sumObjs)
}

class SchedulingInstance(val durations: Array[Int], val demands: Array[Array[Int]], val capacities: Array[Int], val horizon: Int, val precedences: Array[(Int, Int)]) {
  def nTasks = durations.length
  def nResources = capacities.length
}

object BLParser {
  def parse(filePath: String): SchedulingInstance = {

    var lines = Source.fromFile(filePath).getLines.toList.filter(_ != "")

    //first line : nbAct and nbRes
    val firstLineNumbers = lines.head.split(" ")

    val nTasks = firstLineNumbers(0).toInt - 2 //we remove the 2 dummy activities
    val nResources = firstLineNumbers(1).toInt
    val Tasks = 0 until nTasks
    val Resources = 0 until nResources

    lines = lines.drop(1)

    //second line : resources capacities
    val secondLineNumbers = lines.head.split(" ")

    val capacities = Array.tabulate(nResources)(r => {
      secondLineNumbers(r).toInt
    })

    lines = lines.drop(1)

    //next lines are activities descriptions
    //skip the first dummy activity
    lines = lines.drop(1)

    val precedences = new ArrayBuffer[(Int, Int)]()
    val demands = Array.ofDim[Int](nTasks, nResources)
    val durations = Array.ofDim[Int](nTasks)

    for (t <- Tasks) {

      val data = lines.head.split(" ")

      // Demands
      Resources.foreach(r => demands(t)(r) = data(1 + r).toInt)

      // Precedences
      val successors = data.drop(nResources + 2).map(_.toInt - 2).filter(_ != nTasks) //remove 2 from the successors id and get rid of the dummy last activity in the successors
      for (succ <- successors) precedences.append((t, succ))

      // Duration
      durations(t) = data(0).toInt

      //next line
      lines = lines.drop(1)
    }

    new SchedulingInstance(durations, demands, capacities, durations.sum, precedences.toArray)
  }

  def parseBounds(boundsFile: String): (Map[String, Int], Map[String, Int]) = {
    val lines = Source.fromFile(boundsFile).getLines
    // Drop four first lines
    lines.next()
    lines.next()
    lines.next()
    lines.next()
    val lbs = Map[String, Int]()
    val ubs = Map[String, Int]()
    while (lines.hasNext) {
      val line = lines.next
      if (!line.isEmpty()) {
        val data = line.split("\t")
        val instance = data(0)
        val lb = data(1).toInt
        val ub = data(2).toInt
        lbs += instance -> lb
        ubs += instance -> ub
      }
    }
    (lbs, ubs)
  }
}