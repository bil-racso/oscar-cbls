package oscar.lcg.examples

import oscar.lcg._

object Knapsack extends LCGModel with App {

  val costs = Array(6, 5, 4, 3, 2, 1)
  val values = Array(1, 3, 5, 2, 3, 1)
  
  val nItems = costs.length
  val items = Array.tabulate(nItems)(i => BooleanVar("item " + i))
  //val cost = IntVar(7, 8, "cost")
  //val value = IntVar(0, values.sum, "value")
  val cost = IntVar(0, 16, "cost")
  val value = IntVar(12, 15, "value")
  
  add(binaryKnapsack(items, costs, cost))
  add(binaryKnapsack(items, values, value))
  
  onSolution { 
    for (i <- 0 until nItems) {
      if (items(i).value) print(costs(i) + " ")
      else print("0 ")
    }
    println("= " + cost.value)
    for (i <- 0 until nItems) {
      if (items(i).value) print(values(i) + " ")
      else print("0 ")
    }
    println("= " + value.value)
  }
  
  val heuristic = static(items)
  
  search(heuristic)
  
  println("nFails " + bjSearch.nFails)
  println("nNodes " + bjSearch.nNodes)
  println("nSols  " + bjSearch.nSols)
  println("compl  " + bjSearch.completed)
}