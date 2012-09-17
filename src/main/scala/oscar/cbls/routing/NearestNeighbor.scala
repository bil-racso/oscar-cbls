package oscar.cbls.routing

import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.search.SearchEngine


/**
 * only works for singe vehicle
 */
object NearestNeighbor extends SearchEngine{
  def apply(problem:VRP with HopDistance){
    var current:Int = 1
    //initialization: shortest next hop
    for (i <- 1 until problem.N-1){
      val nextForI = selectMin(problem.Next.indices)(
        (j:Int) => problem.getHop(current,j),
        (j:Int) => (problem.Next(j).value==0 && current!=j && j!=0 && j!=1))
      problem.Next(current) := nextForI
      current = nextForI
    }
    problem.Next(current) := 1 //closing the loop
  }
}
