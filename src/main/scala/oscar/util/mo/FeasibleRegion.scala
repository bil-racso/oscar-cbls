package oscar.util.mo

class FeasibleRegion(var feasibilityFunctions: List[Array[Double] => Boolean]) {
  def addFunction(newFunction: Array[Double] => Boolean) = feasibilityFunctions ::= newFunction
  
  def isFeasible(coordinates: Array[Double]): Boolean = {
    for (feasFctIndex <- 0 until feasibilityFunctions.length) {
      if(!feasibilityFunctions(feasFctIndex)(coordinates)) {
        feasibilityFunctions = feasibilityFunctions(feasFctIndex) :: feasibilityFunctions.take(feasFctIndex) ::: feasibilityFunctions.drop(feasFctIndex + 1)
        return false
      }
    }
    true
  }
}

object FeasibleRegion {
  def apply(feasibilityFunctions: List[Array[Double] => Boolean] = List((it: Array[Double]) => true)) = new FeasibleRegion(feasibilityFunctions)
}