package oscar.cp.scheduling.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._

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
  require(n == resources.size)
  
  
  def setup(strength: CPPropagStrength): CPOutcome = {
    priorityL2 = priority
    
    def sdeCB(v: CPIntVar) =  { v.callPropagateWhenBoundsChange(this, false) }
    def resCB(v:CPIntVar)     = { v.callPropagateWhenBind(this) }
    
    starts    foreach sdeCB
    durations foreach sdeCB
    ends      foreach sdeCB
    demands   foreach sdeCB
    sdeCB(capacity)

    resources foreach resCB

    propagate()
  }
  
  
  val byEndMax = Array.tabulate(n)(i => i)
  val byStartMin =  Array.tabulate(n)(i => i)
  
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
    // TODO: filter activities whose demand can be zero
    var i = 0
    while (i < n) {
      startsmin(i) = starts(i).min
      endsmax(i)   = ends(i).max 
      i += 1
    }
    
    // sort by start min and end max ; if permutations already order activities, no need to sort. This happens often enough to check.
    var beg = 0
    while (beg + 1 < n && startsmin(byStartMin(beg)) <= startsmin(byStartMin(beg + 1))) beg += 1
    if (beg < n - 1) 
      scala.util.Sorting.stableSort(byStartMin, startsmin)
      // oscar.algo.SortUtils.stableSort(byStartMin, 0, n, (a: Int, b: Int) => startsmin(a) < startsmin(b))
    
    beg = 0
    while (beg + 1 < n && endsmax(byEndMax(beg)) <= endsmax(byEndMax(beg + 1))) beg += 1
    if (beg < n - 1)
      scala.util.Sorting.stableSort(byEndMax, endsmax)
      // oscar.algo.SortUtils.stableSort(byEndMax,  0, n, (a: Int, b: Int) => endsmax(a)   < endsmax(b))
      
    
    
    
    val C = capacity.max
    
    i = 0
    while (i < n) {
      r(i) = starts(   byStartMin(i))
      p(i) = durations(byStartMin(i))
      d(i) = ends(     byStartMin(i))
      c(i) = demands(  byStartMin(i))

      mandatory(i) = resources(byStartMin(i)).isBoundTo(id)
      optional(i) = !mandatory(i) && resources(byStartMin(i)).hasValue(id)

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
     *  Kameugne11 does not try to get the tightest possible \Theta,
     *  instead it relies on two heuristics at once :
     *  _ take the \Theta with smallest slack, rho
     *  _ take the \Theta with highest density, tau
     *  This actually converges to the same fixpoint as classic EF with optimal \Theta. 
     */
    
    /* 
    // this is an attempt to save on useless iterations,
    // something tells me that most bound activities at the extremities can be ignored,
    // only the boundaries matter  
    val kBeg_ = byEndMax indexWhere { i => !d(i).isBound }
    val kEnd = n - 1 - byEndMax.reverse.indexWhere { i => !d(i).isBound }
    val kBeg =  if (kBeg_ < 0) n else kBeg_
    
    var k = kBeg
    while(k < kEnd) {
    * 
    */
    
    var k = 0
    while(k < n) {
      val du = endsmax(byEndMax(k))                                                                            // du is the upper bound of \Omega
      var Energy = 0 ; var maxEnergy = 0 ; var r_rho = Int.MinValue
      
      i = n - 1                                                                                                // enumerate on i by decreasing est
      while (i >= 0) {
        if (mandatory(i) && dmax(i) <= du) {                                                                   // \Omega is not << i, so task i can be lower bound of \Omega
          Energy += pmin(i) * cmin(i)
          if (Energy > C * (du - rmin(i))) {
            // println("Energy Overload")
            return Failure                                                      // Energy overload on this interval
          }
            
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
          
          // additional detection of the journal version
          if (mandatory(i) &&
              r_rho > Int.MinValue &&
              maxEnergy + cmin(i) * (rmin(i) + pmin(i) - r_rho) > C * (du - r_rho) &&
              Dupd(i) > rmin(i) &&
              r(i).updateMin(Dupd(i)) == Failure)
            return Failure
        }
        
        if (optional(i) && dmax(i) <= du) {                        // if an activity that could be in this resource
          if (Energy + pmin(i) * cmin(i) > C * (du - rmin(i))) {   // would make it overload
            optional(i) = false
            if (resources(byStartMin(i)).removeValue(id) == Failure)   // remove it from this resource
              return Failure
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
              // println(s"* EF Updating ${r(i)} with $newlbpi")
              if (mandatory(i) && r(i).updateMin(newlbpi) == Failure) return Failure
              if (optional(i) && newlbpi > r(i).max) {
                if (resources(byStartMin(i)).removeValue(id) == Failure) return Failure
              }
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
  def apply(s: Array[CPIntVar], d: Array[CPIntVar], e: Array[CPIntVar], dem: Array[CPIntVar], r: Array[CPIntVar], capacity: CPIntVar, id :Int): Array[Constraint] = {
    val rs = s.map(_.opposite).asInstanceOf[Array[CPIntVar]]
    val re = e.map(_.opposite).asInstanceOf[Array[CPIntVar]]
    val priorityL2R = 1
    val priorityR2L = 1
    Array[Constraint](new EFKameugne11WithResources(s,  d, e,  dem, r, capacity, id, priorityL2R),
                      new EFKameugne11WithResources(re, d, rs, dem, r, capacity, id, priorityR2L))
    // new EFKameugne11WithResources(s, d, e, dem, r, capacity, id, priorityL2R)
    // new EFKameugne11WithResources(re, d, rs, dem, r, capacity, id, priorityL2R)
  }
}

