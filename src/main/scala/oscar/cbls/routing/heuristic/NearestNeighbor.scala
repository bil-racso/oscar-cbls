package oscar.cbls.routing.heuristic

import oscar.cbls.search.SearchEngine
import oscar.cbls.routing.{VRP, HopDistance}

/**
 * only works for singe vehicle
 */

object NearestNeighbor extends SearchEngine{
  def apply(problem:VRP with HopDistance){
    var current:Int = 0
    //initialization: shortest next hop
    for (i <- 0 until problem.N-1){
      val nextForI = selectMin(problem.Next.indices)(
        (j:Int) => problem.getHop(current,j),
        (j:Int) => (problem.Next(j).value==j && current!=j && j!=0))
      problem.Next(current) := nextForI
      current = nextForI
    }
    problem.Next(current) := 0 //closing the loop
  }
}
