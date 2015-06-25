package oscar.cp.premulative

import oscar.cp.Constraint
import oscar.cp.CPIntVar
import oscar.cp.core._
import oscar.algo.SortUtils

class Prejunctive(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],pred: Array[Array[Int]]) extends Constraint(starts(0).store) {
  idempotent = false
  priorityL2 = 0
  val nTasks = starts.length
  
  private[this] val runs1 = new Array[Int](nTasks+1)
  private[this] val aux1 = new Array[Int](nTasks)
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    for (i <- 0 until nTasks) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
    }

    propagate()
  }
  
  override def propagate(): CPOutcome = {
    val tasks = (0 until nTasks).toArray
    SortUtils.mergeSort(tasks, starts.map(_.min), 0, tasks.length)
    //pre: assume all tasks are time-propagated
    var i = 0
    while(i < nTasks){
      val t = tasks(i)
      var j = i-1
      var est = starts(t).min
      var dur = 0
      while(j>=0){
        val s = tasks(j)
        if(pred(s)(t) >=0){//s must come before t
          dur += durations(s).value//assumes durations are fixed, here!
          if(dur + starts(s).min > est){
            est = dur + starts(s).min
          }
        }
        j-=1
      }
//      if(est > starts(t).max) return CPOutcome.Failure
//      else if(est > starts(t).min) starts(t).updateMin(est)
      i+=1
    }
    SortUtils.mergeSort(tasks, ends.map(-_.max), 0, tasks.length)
    //pre: assume all tasks are time-propagated
    i = 0
    while(i < nTasks){
      val t = tasks(i)
      var j = i-1
      var lct = ends(t).max
      var dur = 0
      while(j>=0){
        val s = tasks(j)
        if(pred(t)(s) >=0){//s must come after t
          dur += durations(s).value//assumes durations are fixed, here!
          if(ends(s).max - dur < lct){
            lct = ends(s).max - dur
          }
        }
        j-=1
      }
      //if(lct < ends(t).min) return CPOutcome.Failure
      //else if(lct < ends(t).max) ends(t).updateMax(lct)
      i+=1
    }
    
    CPOutcome.Suspend
  }
}