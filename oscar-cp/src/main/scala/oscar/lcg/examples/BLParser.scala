package oscar.lcg.examples

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

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
}