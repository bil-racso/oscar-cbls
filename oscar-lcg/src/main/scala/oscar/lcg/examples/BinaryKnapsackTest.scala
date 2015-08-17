package oscar.lcg.examples

import oscar.lcg._
import oscar.lcg.constraints.BinaryKnapsack

object BinaryKnapsackTest extends LCGModel with App {

  val costs = Array(1, 2, 3, 4, 5)
  val nItems = costs.length
  val Items = 0 until nItems
  val items = Array.tabulate(nItems)(i => BooleanVar("item " + i))
  val cost = IntVar(7, 8, "cost")
  
  add(new BinaryKnapsack(items, costs, cost, "knapsack"))
  
  onSolution { 
    for (i <- 0 until 5) if (items(i).isTrue) print(costs(i) + " ")
    println("= " + cost.value)
  }
  
  val heuristic = static(items)
  
  search(heuristic)
  
  println("nFails " + dfsearch.nFails)
  println("nNodes " + dfsearch.nNodes)
  println("nSols  " + dfsearch.nSols)
  println("compl. " + dfsearch.completed)
}