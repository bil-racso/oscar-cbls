package oscar.lcg.modeling

import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.variables.LCGIntervalVar
import oscar.lcg.constraints.LowerEqual
import oscar.lcg.constraints.DecompCumulative

/** @author Renaud Hartert ren.hartert@gmail.com */
trait Constraints {
  
  def cumulative(starts: Array[LCGIntervalVar], durations: Array[Int], demands: Array[Int], capacity: Int): LCGConstraint = {
    val horizon = starts.maxBy(_.max).max + 1
    new DecompCumulative(starts, durations, demands, capacity, horizon)
  }
  
  def lowerEqual(left: LCGIntervalVar, right: LCGIntervalVar): LCGConstraint = new LowerEqual(left, right)

}