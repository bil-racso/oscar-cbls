package oscar.lcg.examples

import oscar.lcg._

object Knapsack extends LCGModel with App {

  val costs = Array(1, 2, 3, 4, 5)
  val values = Array(5, 4, 3, 2, 1)
  
  val nItems = costs.length
  val items = Array.tabulate(nItems)(i => BooleanVar("item " + i))
  val cost = IntVar(7, 8, "cost")
  val value = IntVar(0, values.sum, "value")
  
  add(binaryKnapsack(items, costs, cost))
  add(binaryKnapsack(items, values, value))
  
  onSolution { 
    for (i <- 0 until 5) if (items(i).isTrue) print(costs(i) + " ")
    print("= " + cost.value)
    println(", value = " + value.value)   
  }
  
  val heuristic = static(items)
  
  search(heuristic)
  
  println("nFails " + dfsearch.nFails)
  println("nNodes " + dfsearch.nNodes)
  println("nSols  " + dfsearch.nSols)
  println("compl  " + dfsearch.completed)
}