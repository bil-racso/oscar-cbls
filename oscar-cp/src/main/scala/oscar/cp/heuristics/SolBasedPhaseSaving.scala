package oscar.cp.heuristics
import oscar.cp.CPIntVar
import oscar.cp.constraints.DiffVal

/** Solution-Based Phase Saving
  * @param variables  The variables on which the variable ordering heuristic is applied
  * @param isMin      True if COP is a minimization problem, false otherwise
  * @param fallBack   Fallback value heuristic
  * @param landscape  returns the landscape score for a (variable, value) pair
  * @author           Yannick Goulleven : yannick.goulleven@student.uclouvain.be
  */
class SolBasedPhaseSaving(variables: Array[CPIntVar], isMin:Boolean, fallBack: Int => Int, landscape: (Int, Int) => Int) {

  private[this] val solValues = Array.ofDim[Int](variables.length)

  // updates the current best solution
  def updateSolution(): Unit = {

    for(i <- variables.indices) {
      solValues(i) = variables(i).getMin
    }
  }

  // adds constraints based on the objective landscape
  def addConstraints(): Unit = {

    for(i <- variables.indices) {
      val solLandscapeValue = landscape(i, solValues(i))
      val values = Array.ofDim[Int](variables(i).getSize)
      variables(i).fillArray(values)
      for(v <- values) {
        if(isMin) {
          if(landscape(i, v) > solLandscapeValue) {
            variables(0).store.post(new DiffVal(variables(i), v))
          }
        }
        else {
          if(landscape(i, v) < solLandscapeValue) {
            variables(0).store.post(new DiffVal(variables(i), v))
          }
        }
      }
    }
  }

  // selects the value from the previous solution if still in the domain of the variable
  // calls the fallback value heuristic otherwise
  def selectValue(i:Int):Int = {
    val r = solValues(i)
    if(variables(i).hasValue(r)) {
      r
    }
    else {
      fallBack(i)
    }
  }

}