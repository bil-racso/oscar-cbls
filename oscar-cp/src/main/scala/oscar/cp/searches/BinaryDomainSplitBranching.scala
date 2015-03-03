package oscar.cp.searches

import oscar.cp._
import oscar.algo.search.Branching

/**
 * Binary search on the decision variables vars, splitting the domain at the selected value (left : <= value, right : > value)
 */
class BinaryDomainSplitBranching(variables: Array[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)) extends BinaryBranching(variables, varHeuris, valHeuris) {

  def this(x: Array[CPIntVar], varHeuris: (Int => Int)) = this(x, varHeuris, i => (x(i).min + x(i).max) / 2)

  final override def alternatives(): Seq[Alternative] = {
    if (allBounds()) noAlternative
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      branch(cp.post(variable <= value))(cp.post(variable > value))
    }
  }
}