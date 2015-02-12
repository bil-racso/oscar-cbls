package oscar.cp.scheduling.constraints

import oscar.algo.SortUtils
import oscar.cp.scheduling.util.OpenSparseSet
import Math.min
import Math.max
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPStore

// Expected usage: update smin/smax...
// Then rebuild profile
// Finally sweep all relevant activities

class ProfileStructure(
   smin: Array[Int], 
   smax: Array[Int], 
   dmin: Array[Int],
   emin: Array[Int],
   emax: Array[Int],
   hmin: Array[Int], 
   required: Array[Boolean], 
   possible: Array[Boolean])(implicit val store: CPStore) {

  private[this] final val nTasks = smax.length
//  private final val Tasks = 0 until nTasks

  private[this] val pointTimes = Array.ofDim[Int](nTasks * 2 + 2)  // one point for origin of times, one for ending
  private[this] val pointHeights = Array.ofDim[Int](nTasks * 2 + 2)
  private[this] var nPoints = 1

  // Initial point
  pointTimes(0) = Int.MinValue
  pointHeights(0) = 0

  private[this] val sortedByStarts = Array.tabulate(nTasks)(i => i)
  private[this] val sortedByEnds   = Array.tabulate(nTasks)(i => i)
  private var nSorted = 0

  private[this] val startVals = Array.ofDim[Int](nTasks)
  private[this] val endVals = Array.ofDim[Int](nTasks)
  
  // for mergeSort
  private val temp1 = Array.ofDim[Int](nTasks + 1)
  private val temp2 = Array.ofDim[Int](nTasks + 1)
  
  private[this] val sortedByStartsStart = new ReversibleInt(store, 0)
  private[this] val sortedByStartsEnd   = new ReversibleInt(store, nTasks)
  private[this] val sortedByEndsStart   = new ReversibleInt(store, 0)
  private[this] val sortedByEndsEnd     = new ReversibleInt(store, nTasks)

  def rebuild2(toConsider: OpenSparseSet): Unit = {
    // Reset
    nPoints = 1
    
    val smax = this.smax
    val emin = this.emin

    // Sort all tasks, useful or not
    var startsStart = sortedByStartsStart.value
    var endsStart   = sortedByEndsStart.value
    
    SortUtils.mergeSort(sortedByStarts, smax, startsStart, nTasks, temp1, temp2)
    SortUtils.mergeSort(sortedByEnds,   emin, endsStart,   nTasks, temp1, temp2)

    val status = toConsider.status
    val limit  = toConsider.limit.value
    
    while (startsStart < nTasks && 
           status(sortedByStarts(startsStart)) >= limit)
      startsStart += 1
    sortedByStartsStart.setValue(startsStart)

    while (endsStart < nTasks && 
           status(sortedByEnds(endsStart)) >= limit)
      endsStart += 1
    sortedByEndsStart.setValue(endsStart)

    var s = startsStart // next start
    var e = endsStart // next end
    var t = Int.MinValue // sweep-line
    var h = 0 // profile height
    
    while (e < nTasks) {
      val prevH = h

      // Move the sweep-line
      t = emin(sortedByEnds(e))
      if (s < nTasks) {
        val start = smax(sortedByStarts(s))
        if (start < t) t = start
      }

      // Process SCP
      while (s < nTasks && smax(sortedByStarts(s)) == t) {
        val ss = sortedByStarts(s)
        if (required(ss) && status(ss) < limit && smax(ss) < emin(ss))  h += hmin(ss)
        s += 1
      }

      // Process ECP
      while (e < nTasks && emin(sortedByEnds(e)) == t) {
        val ee = sortedByEnds(e)
        if (required(ee) && status(ee) < limit && smax(ee) < emin(ee))  h -= hmin(ee)
        e += 1
      }

      // If the profile has changed
      if (h != prevH) {
        pointTimes(nPoints) = t
        pointHeights(nPoints) = h
        nPoints += 1
      }
    }
    // add end of time
    pointTimes(nPoints) = Int.MaxValue
    pointHeights(nPoints) = 0
    nPoints += 1
  }

  
  def rebuild(toConsider: OpenSparseSet): Unit = {
    // Reset
    nPoints = 1
    nSorted = 0

    // TODO: remove need for sorting here if mergesort becomes a bottleneck/costly
        
    // Compute only profile events to consider
    val acts = toConsider.sortedByStatus
    var p = toConsider.limit.value - 1
    while (p >= 0) {
      val i = acts(p)
      if (required(i) && hmin(i) > 0) {
        val start = smax(i)
        val end = emin(i)
      
        if (start < end) {
          sortedByStarts(nSorted) = i
          sortedByEnds(nSorted) = i
          startVals(i) = start
          endVals(i) = end
          nSorted += 1
        }
      }
      p -= 1
    }

    // Sort events
    SortUtils.mergeSort(sortedByStarts, startVals, 0, nSorted, temp1, temp2)
    SortUtils.mergeSort(sortedByEnds,   endVals,   0, nSorted, temp1, temp2)

    var s = 0 // next start
    var e = 0 // next end
    var t = 0 // sweep-line
    var h = 0 // height
    while (e < nSorted) {

      val end = endVals(sortedByEnds(e))
      val prevH = h

      // Move the sweep-line
      if (s == nSorted) t = end
      else {
        val start = startVals(sortedByStarts(s))
        if (start < end) t = start
        else t = end
      }

      // Process SCP
      while (s < nSorted && startVals(sortedByStarts(s)) == t) {
        h += hmin(sortedByStarts(s))
        s += 1
      }

      // Process ECP
      while (e < nSorted && endVals(sortedByEnds(e)) == t) {
        h -= hmin(sortedByEnds(e))
        e += 1
      }

      // If the profile has changed
      if (h != prevH) {
        pointTimes(nPoints) = t
        pointHeights(nPoints) = h
        nPoints += 1
      }
    }
    // add end of time
    pointTimes(nPoints) = Int.MaxValue
    pointHeights(nPoints) = 0
    nPoints += 1
  }

  /*
   * Functions to manage sweeping.
   * Tasks are sweep left to right (right to left) to find left (right) support.
   * LR (RL) sweeping uses indexBefore (indexAfter) which can be linear time
   * in the worst case, though we could use a log n approach.
   * Here we simply cache the last index that worked for every given task,
   * and try to use it as a hint the best we can.
   * We expect that in many cases, the cache will have the exact index.
   */
  
  val lastIndexBefore = Array.fill(nTasks)(0)
  val lastIndexAfter = Array.fill(nTasks)(0)
  
    // returns the profile event at t or the closest before
  @inline private final def indexBefore(a: Int, t: Int): Int = {
    val last = lastIndexBefore(a)
    val now = if (last >= nPoints) indexBeforeLeft(t, nPoints - 1)
    else if (t < pointTimes(last)) indexBeforeLeft(t, last - 1)
    else if (pointTimes(last) < t) indexBeforeRight(t, last + 1)
    else last
    lastIndexBefore(a) = now
    now
  }
  
  @inline private final def indexBeforeLeft(t: Int, hint: Int): Int = {
    var i = hint
    while (t < pointTimes(i))  i -= 1
    i
  }
  
  // returns the profile event at t or the closest before
  @inline private final def indexBeforeRight(t: Int, hint: Int): Int = {
    var i = hint
    while (i < nPoints && pointTimes(i) <= t)  i += 1
    i - 1
  }
  
  // returns the profile event at t or the closest before
  @inline private final def indexAfter(a: Int, t: Int): Int = {
    val last = lastIndexAfter(a)
    val now = if (last >= nPoints) indexAfterLeft(t, nPoints - 1)
    else if (t < pointTimes(last)) indexAfterLeft(t, last - 1)
    else if (pointTimes(last) < t) indexAfterRight(t, last + 1)
    else last
    lastIndexAfter(a) = now
    now
  }

  
  
  // returns the profile event at t or the closest after
  @inline private final def indexAfterLeft(t: Int, hint: Int): Int = {
    var i = hint
    while (i >= 0 && t <= pointTimes(i))  i -= 1
    i + 1
  }
  
  // returns the profile event at t or the closest after
  @inline private final def indexAfterRight(t: Int, hint: Int): Int = {
    var i = hint
    while (pointTimes(i) < t)  i += 1
    i
  }
  
  
  // returns the profile event at t or the closest before
  @inline private final def indexBefore1(t: Int): Int = {
    var i = 1
    while (i < nPoints && pointTimes(i) <= t) {
      i += 1
    } 
    i - 1
  }
  
  // returns the profile event at t or the closest before
  @inline private final def indexAfter1(t: Int): Int = {
    var i = nPoints - 1
    while (i >= 0 && pointTimes(i) > t) {
      i -= 1
    } 
    i + 1
  }
  
  
  // indexBefore by binary search.
  @inline private final def indexBefore2(t: Int): Int = {
    var l = 0
    var r = nPoints - 1
    while (l + 1 < r) {
      val i = (l + r) >> 1   // (l + r) / 2
      if (pointTimes(i) > t) r = i
      else l = i
    }
    l
  }

  // indexAfter by binary search.
  @inline private final def indexAfter2(t: Int): Int = {
    var l = 0
    var r = nPoints - 1
    while (l + 1 < r) {
      val i = (l + r) >> 1   // (l + r) / 2
      if (pointTimes(i) < t) l = i
      else r = i
    }
    r
  }
  
  
  // Given an activity i, check whether it can fit with the mandatory profile at its smin.
  // If it cannot, find the next place where it can.
  // If there is no such place, return something > smax, typically the next profile event
  // In order to detect that a possible activity cannot fit 

  final def sweepLR(a: Int, C: Int, contributes : Boolean): Int = {
    var event  = indexBefore(a, smin(a))  // e = event before or at smin(a)
    
    var checkFrom = Int.MinValue
    val checkUntilDefault =
      if (contributes) smax(a) else max(smax(a) + dmin(a), emin(a))
    var checkUntil = min(checkUntilDefault, emin(a))
    
    while (pointTimes(event) < checkUntil) {
      if (pointHeights(event) > C) {  // always go to conflict
        checkFrom = Int.MaxValue
        checkUntil = checkUntilDefault
      }
      else if (pointTimes(event) < checkFrom) { // if activity was in conflict, go to check
        checkFrom = pointTimes(event)
        val newEMin = max(checkFrom + dmin(a), emin(a)) // if duration is not fixed, maybe emin(a) > smin(a) + dmin(a)
        checkUntil = min(newEMin, checkUntilDefault)
      }
      // otherwise stay in check, nothing to do
      
      event += 1
    }
    
    
    checkFrom
  }


  
  
  // reverse of sweepLR, watch out for < that become >= and the such, since e is not in [s ; e)
  // also, the profile height is at event - 1
  final def sweepRL(a: Int, C: Int, contributes : Boolean): Int = {
    var event  = indexAfter(a, emax(a)) // index at or after emax(a)
    
    var checkFrom = Int.MaxValue  // time where the activity tries to hold right shifted
    val checkUntilDefault =
      if (contributes) emin(a) else min(emin(a) - dmin(a), smax(a))
    var checkUntil = max(checkUntilDefault, smax(a))
    
    while (pointTimes(event) > checkUntil) {
      if (pointHeights(event - 1) > C) {  // always go to conflict
        checkFrom = Int.MinValue
        checkUntil = checkUntilDefault
      }
      else if (pointTimes(event) > checkFrom) { // if activity was in conflict, go to check
        checkFrom = pointTimes(event)
        val newSMax = min(checkFrom - dmin(a), smax(a))  // if duration is not fixed, maybe smax(a) < emax(a) - dmin(a)
        checkUntil = max(newSMax, checkUntilDefault)
      }
      // otherwise stay in check, nothing to do
      
      event -= 1
    }
    
    checkFrom
  }
  
  
  // find minimum height on open interval [time1 ; time2)
  def minInterval(time1: Int, time2: Int): Int = {
    // Find first
    var i = 1
    while (i < nPoints && pointTimes(i) <= time1) i += 1
    i -= 1

    var min = pointHeights(i)

    // Find min
    while (i < nPoints && pointTimes(i) < time2) {
      val h = pointHeights(i)
      if (h < min) min = h
      i += 1
    }

    min
  }
  
  
  def maxInterval(time1: Int, time2: Int): Int = {
    // Find first before time1
    var i = 1
    while (i < nPoints && pointTimes(i) <= time1) i += 1
    i -= 1

    var max = pointHeights(i)

    // Find min
    while (i < nPoints && pointTimes(i) < time2) {
      val h = pointHeights(i)
      if (h > max) max = h
      i += 1
    }

    max
  }
  
  // return the first profile event to happen striclty after t, sweeps right to left
  def pointStrictlyAfter(t: Int) = {
    var et = Int.MaxValue // must be correct if point is at +infinity
    var i = nPoints - 1
    while (i >= 0 && pointTimes(i) > t) {
      et = pointTimes(i)
      i -= 1
    }
    et
  }
  
  
  // return the first profile event to happen striclty after t, sweeps right to left
  def pointStrictlyBefore(t: Int) = {
    var et = Int.MinValue // must be correct if point is at +infinity
    var i = 0
    while (i < nPoints && pointTimes(i) < t) {
      et = pointTimes(i)
      i += 1
    }
    et
  }
  
  
  // returns the maximal height of the whole profile
  def maxHeight(): Int = {
    var max = Int.MinValue
    var i = 0
    while (i < nPoints) {
      if (pointHeights(i) > max) max = pointHeights(i)
      i += 1
    }
    max
  }

  
  def printProfile: Unit = {
    var i = 0
    while (i < nPoints) {
      println(pointTimes(i) + " " + pointHeights(i))
      i += 1
    }
  }
}

