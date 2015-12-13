package oscar.sat.nogoods

import oscar.algo.array.ArrayStack
import oscar.sat.constraints.Clause
import oscar.sat.core.CDCLStore

class NogoodBase(store: CDCLStore) {
  
  // Activity and Decay
  private final val scaleLimit: Double = 1000000
  private[this] var activityStep: Double = 0.5
  private[this] var activityDecay: Double = 0.5
  
  private[this] var learnts: Array[Clause] = new Array(128)
  private[this] var nLearnts: Int = 0
  
  def reduce(): Unit = {
    
    var i = 0
    var j = 0
    var lim = activityStep / learnts.length
    
    // Sort nogoods
    
    while (i < nLearnts / 2) {      
      if (!learnts(i).locked) learnts(i).remove()
      else {
        learnts(j) = learnts(i)
        j += 1
      }     
      i += 1
    }
    
    while (i < learnts.length) {
      if (!learnts(i).locked && learnts(i).activity < lim) learnts(i).remove
      else {
        learnts(j) = learnts(i)
        j += 1
      }     
      i += 1
    }
  }
}