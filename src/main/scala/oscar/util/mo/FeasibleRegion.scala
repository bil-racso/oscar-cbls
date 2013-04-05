package oscar.util.mo

class FeasibleRegion[T, E](var feasibilityFunctions: List[MOOPoint[T, E] => Boolean]) {
  def addFunction(newFunction: MOOPoint[T, E] => Boolean) = feasibilityFunctions ::= newFunction
  
  def isFeasible(point: MOOPoint[T, E]): Boolean = {
    for (feasFctIndex <- 0 until feasibilityFunctions.length) {
      if(!feasibilityFunctions(feasFctIndex)(point)) {
        feasibilityFunctions = feasibilityFunctions(feasFctIndex) :: feasibilityFunctions.take(feasFctIndex) ::: feasibilityFunctions.drop(feasFctIndex + 1)
        return false
      }
    }
    true
  }
}