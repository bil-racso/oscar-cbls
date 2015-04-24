package oscar.cp.scheduling.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import java.lang.Math._
import oscar.cp.StaticCounter

// @author Steven Gay steven.gay@uclouvain.be


/*
 * This cumulative constraint enforces the time-tabling rule.
 * Instead of using Beldiceanu's all-tasks sweeping algorithm,
 * it sweeps every task in turn against the profile.
 * It is generalized to variable durations and optional activities.
 */

class TTPerTask(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
extends CumulativeTemplate(starts, durations, ends, heights, resources, capacity, id, "TTPerTask")
{
  priorityL2 = 4
  idempotent = true

  // Profile
  private final val profile = new ProfileStructure(smin, smax, dmin, emin, emax, hmin, required, possible)  
  
  var C = 0

  var hasChanged = true
  
  final override def propagate(): CPOutcome = {
    
    StaticCounter.cpt += 1
    
    updateCache()
    C = capacity.max
    hasChanged = true
    while (hasChanged) {
      hasChanged = false
      if (oneSweep()) return Failure
    }
    
    removeExtremal()
    if (filterPossibleActivities()) return Failure
    
    Suspend 
  }
  
  final def oneSweep(): Boolean = {
    profile.rebuild(toConsider)
    
    val maxHeight = profile.maxHeight()
    if (maxHeight > C) return true  // check overload 
   
    pushAll(maxHeight)
  }

  
  final def pushAll(maxHeight: Int): Boolean = {
    val minPushable = C - maxHeight
    var p = toConsider.limit.value - 1
    while (p >= 0) {
      val i = activitiesToConsider(p)
      if (required(i) && hmin(i) > minPushable) {
        val aContributes = smax(i) < emin(i)   // compute this before changing bounds
        
        if (smin(i) < smax(i)) {   // push i to the right
          val profileSMin = profile.sweepLR(i, C - hmin(i), aContributes)
          if (!aContributes && profileSMin > smax(i)) return true
          val newSMin = min(profileSMin, smax(i))

          if (newSMin > smin(i)) {
            if (starts(i).updateMin(newSMin) == Failure) return true
            val newEMin = max(newSMin + dmin(i), emin(i))  // emin might already be later if duration is not constant
            if (newEMin > emin(i) && ends(i).updateMin(newEMin) == Failure) return true
            smin(i) = newSMin
            emin(i) = newEMin
            hasChanged |= smax(i) < emin(i)  // do fixed point only if profile changes
          } 
        }

        if (emin(i) < emax(i)) {   // push i to the left
          val profileEMax = profile.sweepRL(i, C - hmin(i), aContributes)
          if (!aContributes && profileEMax < emin(i)) return true
          val newEMax = max(profileEMax, emin(i))
        
          if (newEMax < emax(i)) {
            if (ends(i)  .updateMax(newEMax) == Failure) return true
            val newSMax = min(newEMax - dmin(i), smax(i))
            if (newSMax < smax(i) && starts(i).updateMax(newSMax) == Failure) return true
            emax(i) = newEMax
            smax(i) = newSMax
            hasChanged |= smax(i) < emin(i)
          }
        }
      }
      p -= 1
    }
    false
  }
  
  
  // TODO: use answer from profile.sweepXY to directly remove activity from resource.
  // TODO: actually only one sweep is enough to know whether the activity can hold or not,
  // we don't care about having two witnesses.
  // TODO: separate possible from required (in cumulative template?)
  final def filterPossibleActivities(): Boolean = {
    var p = toConsider.limit.value - 1
    while (p >= 0) {
      val i = activitiesToConsider(p)
      if (possible(i) && !required(i) && hmin(i) > 0) { // && dmin(i) > 0
        
        val newEMin: Int = if (smin(i) < smax(i)) {
          val profileSMin = profile.sweepLR(i, C - hmin(i), false)  // push i to the right
          val newSMin = min(profileSMin, smax(i))

          // if (profileSMin == Int.MaxValue) Int.MinValue else            
          if (newSMin > smin(i)) max(newSMin + dmin(i), emin(i))            
          else emin(i)
          
        }
        else emin(i)
        
        val newSMax: Int = if (emin(i) < emax(i)) {
          val profileEMax = profile.sweepRL(i, C - hmin(i), false)  // push i to the left
          val newEMax = max(profileEMax, emin(i))
          
          // if (profileEMax == Int.MinValue) Int.MaxValue else            
          if (newEMax < emax(i)) min(newEMax - dmin(i), smax(i))            
          else smax(i)
        }
        else smax(i)
        
        if (newSMax < newEMin && profile.maxInterval(newSMax, newEMin) + hmin(i) > C){
          if (resources(i).removeValue(id) == Failure) return true
          possible(i) = false
        } 
      }
      p -= 1
    }
    false
  }
  
  

}


object TTPerTask {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], heights: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint =
    new TTPerTask(starts, durations, ends, heights, resources, capacity, id)
}
