package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength._
import oscar.cp.scheduling.constraints.EFKameugne11WithResources
import oscar.cp.scheduling.constraints.EnergeticChecker

class MaxCumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int = 1)
extends Constraint(starts.head.store, "Max Cumulative") {
  override def setup(l: CPPropagStrength): CPOutcome = {
     if (s.post(new MaxCumulativeCapaCheck(starts,durations,ends,demands,resources,capacity,id)) == Failure) {
       return Failure
     }

    l match {
      case Weak => 
        if (s.post(SweepMaxCumulative(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
        
      case Medium =>
        if (s.post(SweepMaxCumulative(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
        if (s.post(EFKameugne11WithResources(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
//        if (s.post(new EnergeticChecker(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
        
      case Strong =>
        if (s.post(SweepMaxCumulative(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
        if (s.post(EFKameugne11WithResources(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
        // if (s.post(new EnergeticChecker(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
        if (s.post(new EnergeticReasoning(starts,durations,ends,demands,resources,capacity,id)) == Failure) return Failure
    }
    Success
  }
}

object MaxCumulative {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) =
    new MaxCumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
}