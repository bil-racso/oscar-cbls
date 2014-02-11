package oscar.cbls.routing.initial

import oscar.cbls.routing.model.VRP

/**
 * Constructs an initial solution where no point is routed, actually.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
object AllUnrouted {
  def apply(vrp:VRP){
    for (i <- 0 to vrp.V-1){
      vrp.next(i) := i
    }
    for (i <- vrp.V to vrp.N-1){
      vrp.next(i) := vrp.N
    }
  }
}
