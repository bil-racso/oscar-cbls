package oscar.cbls.routing.initial

import oscar.cbls.routing.model.VRP

/**
 * Constructs an initial solution where no point is routed, actually.
 */
object AllUnrouted {
  def apply(vrp:VRP){
    for (i <- 0 to vrp.V-1){
      vrp.Next(i) := i
    }
    for (i <- vrp.V to vrp.N-1){
      vrp.Next(i) := vrp.N
    }
  }
}
