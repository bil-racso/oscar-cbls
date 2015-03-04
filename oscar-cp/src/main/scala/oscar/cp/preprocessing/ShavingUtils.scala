package oscar.cp.preprocessing

import oscar.cp._
import oscar.algo.search.DFSearch
import oscar.cp.core.CPOutcome

/** Renaud Hartert ren.hartert@gmail.com */
object ShavingUtils {

  /** Strenghten the lower bound of the objective by complete shaving on the forwardVariables */
  def strengthenLowerBound(problem: CPStore, forwardVariables: Array[CPIntVar], objective: CPIntVar): CPOutcome = {
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalBound = objective.min
    var bestBound = Int.MaxValue
    search.onSolution {
      val bound = objective.min
      if (bound < bestBound) bestBound = bound
    }
    
    search.start(branching, _ => bestBound == originalBound) 
    
    // Update the bound 
    objective.updateMin(bestBound)
    problem.propagate()
  }
  
  /** Strenghten the upper bound of the objective by complete shaving on the forwardVariables */
  def strengthenUpperBound(problem: CPStore, forwardVariables: Array[CPIntVar], objective: CPIntVar): CPOutcome = {
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalBound = objective.max
    var bestBound = Int.MinValue
    search.onSolution {
      val bound = objective.max
      if (bound > bestBound) bestBound = bound
    }
    
    search.start(branching, _ => bestBound == originalBound) 
    
    // Update the bound
    objective.updateMax(bestBound)
    problem.propagate()
  }
  
  /** Reduce the domain of variables by complete shaving on the forwardVariables */
  def reduceDomains(problem: CPStore, forwardVariables: Array[CPIntVar], variables: Array[CPIntVar]): CPOutcome = {
    
    val nVariables = variables.length
    val Variables = 0 until nVariables
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalSize = Array.tabulate(variables.length)(i => variables(i).size)
    val reducedDomains = Array.fill(variables.length)(Set[Int]())
    
    search.onSolution {
      var i = variables.length
      while (i > 0) {
        i -= 1
        for (value <- variables(i).iterator) reducedDomains(i) += value
      }
    }
    
    search.start(branching, _ => Variables.forall(i => reducedDomains(i).size == originalSize(i)))
    
    // Reduce the domains
    for (i <- Variables) {
      val domain = variables(i).iterator
      for (value <- domain) if (!reducedDomains(i).contains(value)) variables(i).removeValue(value)
    }

    problem.propagate()   
  }
}