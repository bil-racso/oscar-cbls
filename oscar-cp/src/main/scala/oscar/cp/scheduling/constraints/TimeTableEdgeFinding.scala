package oscar.cp.scheduling.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling._
import java.lang.Math._
import oscar.algo.SortUtils._

/*
 * TTEF of Vilim 2011 as described in Schutt et al 2013 
 */

class TimeTableEdgeFinding(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends Constraint(capacity.store, "TimeTableEdgeFinding") {
  private val lr = new TimeTableEdgeFindingLR(starts, durations, ends, heights, resources, capacity, id) 
  private val rl = new TimeTableEdgeFindingLR(ends map(-_), durations, starts map(-_), heights, resources, capacity, id)
  private val store = capacity.store
  
  override def setup(strength: CPPropagStrength) = {
    try {
      if (store.post(Array[Constraint](lr, rl)) == Failure) Failure
      else Suspend
    }
    catch {
      case e: NoSolutionException => Failure
    }
  }
}


final class TimeTableEdgeFindingLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "TimeTableEdgeFinding")
{
  priorityL2 = 1
  private[this] val nTasks = starts.length

  private[this] var C = 0L  // resource capacity

  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedBySMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val activeBySMin = Array.ofDim[Int](nTasks)
  private[this] val activeByEMax = Array.ofDim[Int](nTasks)
  private[this] val mandatoryBySMax = Array.ofDim[Int](nTasks)
  private[this] val mandatoryByEMin = Array.ofDim[Int](nTasks)
  
  private[this] val indices = new Array[Int](nTasks)
  
  // filters out tasks that should not be considered, then sorts them. Returns the number of items in filtered list.
  private final def filterActiveSort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Int = {
    val nKeys = byKey.length
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = 0 
    var count = 0
    
    // extract only tasks to consider
    while (p < nKeys) {
      val task = byKey(p)
      if (status(task) < limit) {
        filtered(count) = task
        indices(count) = p
        count += 1
      }
      p += 1
    }
    
    // sort them
    mergeSort(filtered, keys, 0, count, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = count
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
    
    count
  }
  
  
  private final def filterMandatorySort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Int = {
    val nKeys = byKey.length
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = 0 
    var count = 0
    
    // extract only tasks to consider, that are required and have a fixed part
    while (p < nKeys) {
      val task = byKey(p)
      if (status(task) < limit && required(task) && smax(task) < emin(task)) {
        filtered(count) = task
        indices(count) = p
        count += 1
      }
      p += 1
    }
    
    // sort them
    mergeSort(filtered, keys, 0, count, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = count
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
    
    count
  }
  
  private[this] val smaxEF = Array.ofDim[Int](nTasks)
  private[this] val eEF    = Array.ofDim[Long](nTasks)
  
  //  TT structure, needed by ttEn primitive
  private[this] val ttBeforeSMin = Array.ofDim[Long](nTasks)
  private[this] val ttBeforeEMax = Array.ofDim[Long](nTasks)
  
  @inline private final def ttEn(a: Int, b: Int): Long = ttBeforeEMax(b) - ttBeforeSMin(a)
  
  final override def propagate(): CPOutcome = {
    updateCache()
    C = capacity.max
    
    removeExtremal() // faster
    //removeImpossible()
    
    val limit = toConsider.limit.value    
    
    val nActive = filterActiveSort(sortedBySMin, activeBySMin, smin)
                  filterActiveSort(sortedByEMax, activeByEMax, emax)
    val nMandatory = filterMandatorySort(sortedByEMin, mandatoryByEMin, emin)
                     filterMandatorySort(sortedBySMax, mandatoryBySMax, smax)
    
    // initialize TT : ttBefore smin/emax
    var sminp, smaxp, eminp, emaxp = 0
    var minHeight, e = 0L
    var maxHeight = 0
    var date, prevDate = if (nActive > 0) smin(activeBySMin(0)) else 0  // first event should be first smin
    
    while (emaxp < nActive) {  // do all events until last one = last emax
      // find next date
      date = emax(activeByEMax(emaxp))
      if (sminp < nActive)      date = min(date, smin(activeBySMin(sminp)))
      if (smaxp < nMandatory) date = min(date, smax(mandatoryBySMax(smaxp)))
      if (eminp < nMandatory) date = min(date, emin(mandatoryByEMin(eminp)))
      
      // update energy
      //e += minHeight * (date - prevDate)
      e += (minHeight + max(0L, C - maxHeight)) * (date - prevDate)
      
      // process events 
      while (sminp < nActive && smin(activeBySMin(sminp)) == date) {
        val a = activeBySMin(sminp)
        ttBeforeSMin(a) = e
        maxHeight += hmin(a)
        sminp += 1
      }
      
      while (emaxp < nActive && emax(activeByEMax(emaxp)) == date) {
        val a = activeByEMax(emaxp)
        ttBeforeEMax(a) = e
        maxHeight -= hmin(a)
        emaxp += 1
      }
      
      while (smaxp < nMandatory && smax(mandatoryBySMax(smaxp)) == date) {
        val a = mandatoryBySMax(smaxp)
        minHeight += hmin(a)
        smaxp += 1
      }
      
      while (eminp < nMandatory && emin(mandatoryByEMin(eminp)) == date) {
        val a = mandatoryByEMin(eminp)
        minHeight -= hmin(a)
        eminp += 1
      }
      
      prevDate = date
    }
    
    // initialize free parts
    var i = nActive
    while (i > 0) {
      i -= 1
      val a = activeBySMin(i)
      
      if (smax(a) < emin(a)) {
        smaxEF(a) = emin(a)
        eEF(a) = hmin(a).toLong * (dmin(a) - (emin(a) - smax(a)))
      }
      else {
        smaxEF(a) = smax(a)
        eEF(a) = hmin(a).toLong * dmin(a)
      }
    }

    var oldMaxX = nActive - 1
    var end = Int.MaxValue
    
    var y = nActive - 1
    while (y >= 0) {
      val b = activeByEMax(y)
      
      end = emax(b) 
      var minBegin = Int.MaxValue ; var u = -1
      var E, enReqU = 0L
      
      while (oldMaxX >= 0 && smin(activeBySMin(oldMaxX)) >= end) oldMaxX -= 1
      
      var x = oldMaxX
      while (x >= 0) {
        val a = activeBySMin(x)
        val begin = smin(a)
        
        if (emax(a) <= end) E += eEF(a)  // the whole free task is in [ begin ; end [
        else {
          val enIn = hmin(a).toLong * max(0, end - smaxEF(a))   // minimal length, rightmost placement in [ begin ; end [
          E += enIn
          val enReqA = min(eEF(a), hmin(a).toLong * (end - smin(a))) - enIn  // additional if leftmost placement
          if (enReqA > enReqU) {
            u = a
            enReqU = enReqA
          }
        }
      
        val reserve = C * (end - begin) - E - ttEn(a, b)
        if (reserve < 0) return Failure
        
        if (reserve < enReqU) {
          val newSMin = min(end, smax(u)) - reserve / hmin(u) 
          
          if (smin(u) < newSMin && starts(u).updateMin(newSMin.toInt) == Failure) return Failure
        }

        x -= 1
      }
      
      do { y -= 1 } while (y > 0 && emax(activeByEMax(y)) == end)   // find next emax
    }
    
    Suspend
  }
  
}


object TimeTableEdgeFinding {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) =
    new TimeTableEdgeFinding(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
}
