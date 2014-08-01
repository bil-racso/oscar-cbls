package oscar.cp.scheduling.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import scala.util.Sorting

// @author Steven Gay steven.gay@uclouvain.be

/*
 * Cumulative extended edge finding as in Kameugne et al. CP2011, excpet for optional activities.
 * Activities may have alternative resources:
 * this version of the propgator removes optional activities that can not fit among mandatory activities.
 */

class EFKameugne11WithResources(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],
                              demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int, priority: Int = 0)
extends Constraint(capacity.store, "EFKameugne11WithResources") {
  val n = starts.size
  require(n == durations.size)
  require(n == ends.size)
  require(n == demands.size)
  
  
  def setup(strength: CPPropagStrength): CPOutcome = {
    priorityL2 = priority
    
    def sdeCB(v: CPIntVar) =  { v.callPropagateWhenBoundsChange(this, false) }
    def resCB(v:CPIntVar)     = { v.callPropagateWhenBind(this) }
    
    starts    foreach sdeCB
    durations foreach sdeCB
    ends      foreach sdeCB
    demands   foreach sdeCB
    resources foreach resCB
    sdeCB(capacity)
    
    propagate()
  }
  
  
  val order = Array.tabulate(n)(i => i)
  
  val r = starts   .map(i => i)
  val p = durations.map(i => i)
  val d = ends     .map(i => i)
  val c = demands  .map(i => i)
  
  val startsmin = Array.fill(n)(0)
  val endsmax   = Array.fill(n)(0)  

  val rmin    = Array.fill(n)(0)
  val dmax    = Array.fill(n)(0)
  val pmin    = Array.fill(n)(0)
  val cmin    = Array.fill(n)(0)
  
  val mandatory = Array.fill(n)(false)
  val optional  = Array.fill(n)(false)
  
  val Dupd    = Array.fill(n)(0)
  val SLupd   = Array.fill(n)(0)
  val E       = Array.fill(n)(0) 
    
  override def propagate(): CPOutcome = {
    // TODO: add resources, filter activities whose demand can be zero
    
    var i = 0
    while (i < n) {
      startsmin(i) = starts(i).min
      endsmax(i)   = ends(i).max 
      i += 1
    }
    
    val byEndMax = order.sortBy(endsmax)
    val reorder  = order.sortBy(startsmin)
    val C = capacity.max
    
    i = 0
    while (i < n) {
      r(i) = starts(   reorder(i))
      p(i) = durations(reorder(i))
      d(i) = ends(     reorder(i))
      c(i) = demands(  reorder(i))

      mandatory(i) = resources(reorder(i)).isBoundTo(id)
      optional(i) = !mandatory(i) && resources(reorder(i)).hasValue(id)

      rmin(i) = r(i).min
      dmax(i) = d(i).max
      pmin(i) = p(i).min
      cmin(i) = c(i).min
      
      Dupd(i)  = Int.MinValue
      SLupd(i) = Int.MinValue
      
      i += 1
    }
    
    /*
     *  Edge Finding tries to find a task interval \Omega and a task i
     *  s.t. \Omega << i, i.e. all tasks of \Omega finish before the end of i.
     *  Then it updates the est of i using a task interval \Theta \subseteq \Omega.
     *  Kameugne11 is does not try to get the tightest possible \Theta,
     *  instead it relies on two heuristics at once :
     *  _ take the \Theta with smallest slack, rho
     *  _ take the \Theta with highest density, tau
     *  This actually converges to the same fixpoint as classic EF with optimal \Theta. 
     */
    
    var k = 0
    while(k < n) {
      val du = endsmax(byEndMax(k))                                                                            // du is the upper bound of \Omega
      var Energy = 0 ; var maxEnergy = 0 ; var r_rho = Int.MinValue
      
      i = n - 1                                                                                                // enumerate on i by decreasing est
      while (i >= 0) {
        if (mandatory(i) && dmax(i) <= du) {                                                                   // \Omega is not << i, so task i can be lower bound of \Omega
          Energy += pmin(i) * cmin(i)
          if ((r_rho == Int.MinValue && Energy > 0) || Energy * (du - r_rho) > maxEnergy * (du - rmin(i))) {   // Energy / (du-rmin(i)) > maxEnergy/(du-r_rho) => i has higher density
            maxEnergy = Energy
            r_rho = rmin(i)
          }
        }
        else if (dmax(i) > du) {                                                                               // \Omega << i automatically: compute the density update for i.
          val rest = if (r_rho == Int.MinValue) r_rho else maxEnergy - (C - cmin(i)) * (du - r_rho)            // Why not wait for density updates from i' < i?
          if (rest > 0) {                                                                                      // We do density updates only for \Theta \subseteq est_i, lct_i 
            Dupd(i) = math.max(Dupd(i), r_rho + ceiling_div(rest, cmin(i))) 
          }
        }
        
        E(i) = Energy
        i -= 1
      }
      
      var minSL = Int.MaxValue ; var r_tau = du
      i = 0
      while (i < n) {
        if (mandatory(i) && C * (du - rmin(i)) - E(i) < minSL) {
          r_tau = rmin(i)
          minSL = C * (du - r_tau) - E(i) 
        }
        
        if (dmax(i) > du) {
          val restp = cmin(i) * (du - r_tau) - minSL
          if (r_tau <= du && restp > 0)
            SLupd(i) = math.max(SLupd(i), r_tau + ceiling_div(restp, cmin(i)))
            
          if (rmin(i) + pmin(i) >= du || minSL < cmin(i) * pmin(i)) {
            val newlbpi = math.max(Dupd(i), SLupd(i))
                        
            if (newlbpi > rmin(i)) {
//              println(s"* EF Updating ${r(i)} with $newlbpi")
              if (mandatory(i) && r(i).updateMin(newlbpi) == Failure) return Failure
              if (optional(i) && newlbpi > r(i).max && resources(reorder(i)).removeValue(id) == Failure) return Failure
            }
          }
        }
        
        i += 1
      }
      k += 1
    }
    
    Suspend
  }
  
  
    // Taken from CPIntVarViewTimes
	@inline
	def ceiling_div(a: Int, b:Int) = {
      val q = a / b
      if (a > 0 && q * b != a) q + 1
      else q
    }

 
}


object EFKameugne11WithResources {
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar], dem: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id :Int)(implicit store: CPStore) = {
    val rs = s.map(_.opposite).asInstanceOf[Array[CPIntVar]]
    val re = e.map(_.opposite).asInstanceOf[Array[CPIntVar]]
    val priorityL2R = 1
    val priorityR2L = 0
    Array(new EFKameugne11WithResources(s,  d, e,  dem, r, capacity, id, priorityL2R),
          new EFKameugne11WithResources(re, d, rs, dem, r, capacity, id, priorityR2L))
    // new EFKameugne11WithResources(s, d, e, dem, r, capacity, id, priorityL2R)
  }
}

