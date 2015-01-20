package oscar.cp.scheduling.constraints

import oscar.cp.core.CPOutcome._
import oscar.algo.SortUtils._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.scheduling.util.OpenSparseSet
import Math._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPIntervalVar
import oscar.cp.core.Constraint

// @author Steven Gay steven.gay@uclouvain.be


class CumulativeTemplate(starts: Array[_ <: CPIntervalVar], durations: Array[_ <: CPIntervalVar], ends: Array[_ <: CPIntervalVar],
                         heights: Array[_ <: CPIntervalVar], resources: Array[CPIntVar], capacity: CPIntervalVar, id: Int, name: String = "Cumulative")
extends Constraint(capacity.store, name) {
  val n = starts.length
  require(n == durations.length)
  require(n == ends.length)
  require(n == heights.length)
  require(n == resources.length)

    
  def setup(strength: CPPropagStrength): CPOutcome = {
    def callbacks(a: Int) = {
      if (!resources(a).isBound) resources(a).callPropagateWhenBind(this)
      
      if (!starts(a).isBound)    starts(a)   .callPropagateWhenBoundsChange(this)
      if (!durations(a).isBound) durations(a).callPropagateWhenBoundsChange(this)
      if (!ends(a).isBound)      ends(a)     .callPropagateWhenBoundsChange(this)
      if (!heights(a).isBound)   heights(a)  .callPropagateWhenBoundsChange(this)
    }
    
    val myActs = (0 until n) filter { a => resources(a).hasValue(id) && heights(a).max > 0 }
    myActs foreach callbacks
    
    if (!capacity.isBound) capacity.callPropagateWhenBoundsChange(this)
    
    propagate()
  }
  
  implicit val store = capacity.store
  
  protected[this] final val smin = Array.fill(n)(0)
  protected[this] final val smax = Array.fill(n)(0)
  protected[this] final val emin = Array.fill(n)(0)
  protected[this] final val emax = Array.fill(n)(0)
  protected[this] final val hmin = Array.fill(n)(0)
  protected[this] final val hmax = Array.fill(n)(0)
  protected[this] final val dmin = Array.fill(n)(0)
  protected[this] final val dmax = Array.fill(n)(0)
  protected[this] final val required = Array.fill(n)(false)
  protected[this] final val possible = Array.fill(n)(false)
  
  protected[this] final val rToUpdate = new OpenSparseSet(n)
  protected[this] final val rByStatus = rToUpdate.sortedByStatus
  
  protected[this] final val hToUpdate = new OpenSparseSet(n)
  protected[this] final val hByStatus = hToUpdate.sortedByStatus
  
  protected[this] final val dToUpdate = new OpenSparseSet(n)  // durations decoupled from s/e
  protected[this] final val dByStatus = dToUpdate.sortedByStatus
  
  protected[this] final val tToUpdate = new OpenSparseSet(n)  // time variables: s/e coupled since constant durations are common
  protected[this] final val tByStatus = tToUpdate.sortedByStatus
  
  class OpenSparseSetMod(n: Int) extends OpenSparseSet(n: Int) {
    override def exclude(a: Int) = {
      super.exclude(a)
      rToUpdate.exclude(a)
      hToUpdate.exclude(a)
      dToUpdate.exclude(a)
      tToUpdate.exclude(a)
    } 
  }
  
  val toConsider = new OpenSparseSetMod(n)
  val activitiesToConsider = toConsider.sortedByStatus


  final def updateResource() = {
    var p = rToUpdate.limit.value - 1
    while (p >= 0) {
      val a = rByStatus(p)
      required(a) = resources(a).isBoundTo(id)
      
      if (required(a)) {
        rToUpdate.exclude(a)
        possible(a) = true
      }
      else if (!resources(a).hasValue(id)) {
        toConsider.exclude(a)
        possible(a) = false
      }
      else {
        possible(a) = true
      }
      p -= 1
    }
  }
  
  
  final def updateHeights() = {
    var p = hToUpdate.limit.value - 1
    while (p >= 0) {
      val a = hByStatus(p)
      hmax(a) = heights(a).max
      
      if (hmax(a) == 0) {
        toConsider.exclude(a)
      }
      else {
        hmin(a) = heights(a).min
        if (hmin(a) == hmax(a)) hToUpdate.exclude(a)
      }
      p -= 1
    }
  }
  
  
  final def updateDurations() = {
    var p = dToUpdate.limit.value - 1
    while (p >= 0) {
      val a = dByStatus(p)
      dmax(a) = durations(a).max
      
      if (dmax(a) == 0) {
        toConsider.exclude(a)
      }
      else {
        dmin(a) = durations(a).min
        if (dmin(a) == dmax(a)) dToUpdate.exclude(a)
      }
      p -= 1
    }
  }
  
  val identity = Array.tabulate(n)(i => i)
  
  final def updateStartsEnds() = {
    var p = tToUpdate.limit.value - 1
    
    while (p >= 0) {
      val a = tByStatus(p)

      if (dmin(a) == dmax(a)) { // s and e are strongly linked
        smin(a) = starts(a).min
        emax(a) = ends(a).max
      
        smax(a) = emax(a) - dmin(a) 
        emin(a) = smin(a) + dmin(a)
      }
      else {
        smin(a) = starts(a).min
        emax(a) = ends(a).max
      
        smax(a) = starts(a).max 
        emin(a) = ends(a).min
      }
      
      if (smin(a) + dmin(a) == emax(a)) {
        tToUpdate.exclude(a)
      }
      p -= 1
    }
  }
  
  def updateCache() = {
    // Step 1: filter out activities not in this resource
    updateResource()
    
    // Step 2: update heights, filter out activities of height 0
    updateHeights()
    
    // Step 3: update durations, 
    // TODO: basically the same as heights... do a common class? 
    // Exclusion based on height could be done in a second step...
    // It is the same as exclusion on duration!
    updateDurations()
    
    // Step 4: update starts/ends
    // We link the update for starts and ends because it is very common
    // we could also split the two if starts and ends often became bound independently.
    updateStartsEnds()
  }
  
  
  
  final def removeExtremal() = {
    // remove extremal activities from consideration, TT dominance.
    // get smin and emax of the task set formed by unfixed activities.  
    var minSMinNotFixed = Int.MaxValue
    var maxEMaxNotFixed = Int.MinValue
    val limit = toConsider.limit.value - 1
    var p = limit
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (smin(a) + dmin(a) < emax(a)) {
        minSMinNotFixed = min(minSMinNotFixed, smin(a))
        maxEMaxNotFixed = max(maxEMaxNotFixed, emax(a))
      }
      p -= 1
    }
      
    // exclude from consideration all activities that are strictly before minSMin
    p = limit
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (required(a) && hmin(a) == hmax(a)) {// && smin(a) + dmin(a) == emax(a)) {
        if (emax(a) <= minSMinNotFixed || smin(a) >= maxEMaxNotFixed) {
          toConsider.exclude(a)
        }
      }
      p -= 1
    }
  }
  
  
  final def removeOneStepExtremal() =  {
    val limit = toConsider.limit.value 
    
    var minSMinNotFixed = Int.MaxValue
    var maxEMaxNotFixed = Int.MinValue
    
    var p = limit - 1
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (smin(a) + dmin(a) < emax(a)) {
        minSMinNotFixed = min(minSMinNotFixed, smin(a))
        maxEMaxNotFixed = max(maxEMaxNotFixed, emax(a))
      }
      p -= 1
    }
    
    // Step 4.2: get minimal smin of activities that overlap minSMinFixed
    var minSMin = minSMinNotFixed
    var maxEMax = maxEMaxNotFixed
    p = limit - 1
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (smin(a) < minSMinNotFixed && minSMinNotFixed < emax(a))
        minSMin = min(minSMin, smin(a))
        
      if (smin(a) < maxEMaxNotFixed && maxEMaxNotFixed < emax(a))
        maxEMax = max(maxEMax, emax(a))
      p -= 1
    }
        
    // Step 4.3: exclude from consideration all activities that are strictly before minSMin
    p = limit - 1
    while (p >= 0) {
      val a = activitiesToConsider(p)
      if (emax(a) <= minSMin) toConsider.exclude(a)
      else if (smin(a) >= maxEMax) toConsider.exclude(a)
      p -= 1
    }
    
  }

}
