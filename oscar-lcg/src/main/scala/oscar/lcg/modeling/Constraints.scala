package oscar.lcg.modeling

import oscar.lcg.variables.IntVar
import oscar.lcg.variables.BooleanVar
import oscar.lcg.core.Constraint
import oscar.lcg.constraints.BinaryKnapsack

trait Constraints {
    
  final def binaryKnapsack(items: Array[BooleanVar], costs: Array[Int], cost: IntVar): Constraint = {
    new BinaryKnapsack(items, costs, cost, "binaryKnapsack")
  }

  final def binaryKnapsack(items: Array[BooleanVar], costs: Array[Int], values: Array[Int], cost: IntVar, value: IntVar): Array[Constraint] = {
    val knapsack1 = new BinaryKnapsack(items, costs, cost, "binaryKnapsack_cost")
    val knapsack2 = new BinaryKnapsack(items, values, value, "binaryKnapsack_value")
    Array(knapsack1, knapsack2)
  }
}