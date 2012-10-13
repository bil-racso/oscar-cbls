package oscar.cbls.routing

import oscar.cbls.search.SearchEngine
import scala.util.Random

/**
 * only works for singe vehicle
 */
object RandomNeighboor extends SearchEngine{
  def apply(problem:VRP with HopDistance){
    var current:Int = 0
    //initialization: random next hop

    val random= Random.shuffle((problem.Next.indices) drop 1)
    for (i <- 0 until problem.N-1){
      problem.Next(current) := random(i)
      current = random(i)
      }
    problem.Next(current) := 0 //closing the loop
  }
}
