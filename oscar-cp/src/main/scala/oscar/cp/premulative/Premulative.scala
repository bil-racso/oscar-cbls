package oscar.cp.premulative


import oscar.cp.core._
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import java.lang.Math._
import oscar.algo.SortUtils
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.constraints.Minimum

/**
 * @author Pierre Schaus
 */

class Premulative(startsArg: Array[CPIntVar], durationsArg: Array[CPIntVar], endsArg: Array[CPIntVar], demandsArg: Array[CPIntVar], resourcesArg: Array[CPIntVar], capaArg: CPIntVar, id: Int,delayMatrix: Array[Array[Int]]) extends Constraint(startsArg.head.store, "Premulative") {

  
  
  
  val args = Array.tabulate(startsArg.size)(i => i).filter(i => demandsArg(i).max > 0 && durationsArg(i).max > 0 && resourcesArg(i).hasValue(id))
  private[this] val starts = args.map(startsArg(_))
  private[this] val ends = args.map(endsArg(_))
  private[this] val durations = args.map(durationsArg(_))
  private[this] val demands = args.map(demandsArg(_))
  
  priorityL2 = 4
  idempotent = true

  private[this] val n = starts.size
  
  val pred = for (i <- args) yield args.map(j => delayMatrix(i)(j))
  
  // start(i) + w(i)(j) <= start(j)
  val precedences: Array[Array[(Int,Int)]] = Array.tabulate(n)(i => Array.tabulate(n)(j => (j,pred(j)(i))).filter(_._2 > 0))
  
  
  
  val totalPredEnergy: Array[Int] = Array.fill(n)(0)
  for (i <- 0 until n; (j,w) <- precedences(i)) {
    totalPredEnergy(i) += Math.min(durations(j).min,w)*demands(j).min
  }
  //, (jmap{  case(j,w) => (durations(j) min w)*demands(j)}.sum) // maybe need ceiling here
  println("totalEnergyPred:"+totalPredEnergy.mkString(","))

  
  
  
  
  
  private[this] val hasManda = Array.fill(n)(false)
  private[this] val isBound = Array.fill(n)(false)
  private[this] val startMin = Array.fill(n)(0)
  private[this] val startMax = Array.fill(n)(0)
  private[this] val endMin = Array.fill(n)(0)
  private[this] val endMax = Array.fill(n)(0)
  private[this] val endMaxMinusOne = Array.fill(n)(0)
  private[this] val dur = Array.fill(n)(0)
  private[this] var capa = capaArg.max

  private[this] val demand = Array.fill(n)(0)
  
  
  private[this] final val required = Array.fill(n)(false)
  private[this] final val possible = Array.fill(n)(false)
  // indices of activities that still need to be considered to update the bounds
  private[this] val toConsider =  new ReversibleSparseSet(s,0,n-1) 
  // indices of activities that still need to be considered when computing the profile
  private[this] val toConsiderInProfile = new ReversibleSparseSet(s,0,n-1) 
  
  private[this] val startMinIndex = Array.fill(n)(0)
  private[this] val endMaxIndex = Array.fill(n)(0)

  private[this] val mandaMin = Array.fill(n)(0)
  private[this] val mandaMax = Array.fill(n)(0)
  private[this] val mandaHeight = Array.fill(n)(0)

  private[this] val profileMin = Array.fill(2 * n + 2)(0)
  private[this] val profileMax = Array.fill(2 * n + 2)(0)
  private[this] val profileHeight = Array.fill(2 * n + 2)(0)
  private[this] var nProfile = 0
  
  
  private[this] var minNotBoundStartMin = Int.MaxValue
  private[this] var maxNotBoundEndMax = Int.MinValue  
  
  
  private[this] val values = Array.ofDim[Int](n)
  
  // retrieve the index of startMin(i) in the profile
  def indexStartMin(i: Int): Int = {
    val value = startMin(i)
    val profileIndex = startMinIndex(i)
    if (profileIndex >= nProfile || profileMin(profileIndex) > value || profileMax(profileIndex) < value) {
      startMinIndex(i) = find(value)
    }
    startMinIndex(i)
  }
  
  // retrieve the index of endMax(i)-1 in the profile
  def indexEndMaxMinusOne(i: Int): Int = {
    val value = endMax(i)-1
    val profileIndex = endMaxIndex(i)
    if (profileIndex >= nProfile || profileMin(profileIndex) > value || profileMax(profileIndex) < value) {
      endMaxIndex(i) = find(value)
    }
    endMaxIndex(i)
  }  
  
  // dichotomic search
  def find(t: Int): Int = {
    var low = 0
    var up = nProfile - 1
    while (low < up) {
      val guess = (low + up) / 2
      if (t < profileMin(guess)) {
        up = guess - 1
      } else if (t > profileMax(guess)) {
        low = guess + 1
      } else {
        low = guess
        up = guess
      }
    }
    return low
  }  
  
  
  private[this] val profilePoint = Array.fill(n * 2 +2)(0)
  private[this] val profileIncr = Array.fill(n * 2 +2)(0)
  private[this] var index = Array.tabulate(2 * n +2 )(i => i)
  private[this] val runs1 = new Array[Int](index.length+1)
  private[this] val aux1 = new Array[Int](index.length)
  
  private var npOld = 0
  def updateProfile(): Boolean = {
    var np = 0 // number of time points in the profile
    
    // add (- infinity)
    profilePoint(np) = Int.MinValue
    profileIncr(np) = 0
    np += 1
    
    val m = toConsiderInProfile.size
    var j = 0
    while (j < m) {
      val i = toConsiderInProfile(j)
      if (hasManda(i)) {
        profilePoint(np) = mandaMin(i)
        profileIncr(np) = mandaHeight(i)
        np += 1
        profilePoint(np) = mandaMax(i) + 1
        profileIncr(np) = -mandaHeight(i)
        np += 1
      }
      j += 1
    }
    
    // (add + infinity)
    profilePoint(np) = Int.MaxValue
    profileIncr(np) = 0
    np += 1
    
    if (np != npOld) {
      j = 0
      while (j < np) {
        index(j) = j
        j += 1
      }
      npOld = np
    }
    
    SortUtils.mergeSort(index, profilePoint, 0, np,runs1,aux1)
    
    nProfile = 0
 
    var t = profilePoint(index(0))
    var h = 0
    var hprev = -1
    var i = 0

    while (i < np) {     
      while (i < np && profilePoint(index(i)) == t) {
        h += profileIncr(index(i))
        i += 1
      }
      if (i < np) {
        if (h != hprev) {
          if (h > capa) {
            return false 
          }
          profileHeight(nProfile) = h
          profileMin(nProfile) = t 
          hprev = h
          nProfile += 1
        }
        t = profilePoint(index(i))
      } else {  
        assert(h == 0)
      }
    }
    i = 0
    while (i < nProfile-1) {
      profileMax(i) = profileMin(i+1)-1
      i += 1
    }
    profileMax(nProfile-1) = Int.MaxValue
    return true
  }
  



  

  
  def updateBounds() { 
    capa = capaArg.max
    minNotBoundStartMin = Int.MaxValue
    maxNotBoundEndMax = Int.MinValue
    val m = toConsider.fillArray(values)
    var j = 0
    while (j < m) {
      val i = values(j)
      j += 1
      updateBound(i,starts(i).min,ends(i).max,durations(i).min,demands(i).min)
      if (isBound(i)) { 
       toConsider.removeValue(i) 
      }
      else {
        minNotBoundStartMin = math.min(minNotBoundStartMin,startMin(i))
        maxNotBoundEndMax = math.max(maxNotBoundEndMax,endMax(i))
      }
    }
  }
  
  
  def updateBound(i: Int,sMin: Int, eMax: Int, durMin: Int, demMin: Int) {
      
      demand(i) = demMin
      startMin(i) = sMin      
      endMax(i) = eMax
      endMaxMinusOne(i) = endMax(i)-1
      val d = durMin
      startMax(i) = endMax(i) - d
      endMin(i) = startMin(i) + d
      isBound(i) = startMin(i) == startMax(i)
      hasManda(i) = startMax(i) < endMin(i) 
      dur(i) = durMin
      if (hasManda(i)) {
        mandaMin(i) = startMax(i)
        mandaMax(i) = endMin(i) - 1
        mandaHeight(i) = demand(i)
      }  
  }
  
  
  def cleanUseless() {
    var j = 0
    val m = toConsiderInProfile.fillArray(values)
    while (j < m) {
      val i = values(j)
      if (endMax(i) <= minNotBoundStartMin || startMin(i) >= maxNotBoundEndMax) {
        toConsiderInProfile.removeValue(i)
      }
      j += 1
    }
  }
  
  override def setup(l: CPPropagStrength): CPOutcome = {

    
    //println(precedences.map(_.size).mkString(","))
    for (i <- 0 until n; if (precedences(i).size > 0)) {
      
      
      import oscar.cp.modeling._
      val minEarliestPred: CPIntVar = minimum(precedences(i).map{case (j,w) => starts(j)})
      
      // 

      
      val minLagEarliestPred = totalPredEnergy(i)/capa + (if (totalPredEnergy(i) % capa + demands(i).min > capa) 1 else 0)
      
      println("capa max "+capa)
      println("predecessors of "+i+"="+precedences(i).map(a => (a,durations(a._1).min,demands(a._1).min)).mkString(",")+" minLag:"+minLagEarliestPred)
      
      s.post(minEarliestPred + minLagEarliestPred <= starts(i))
      
    }
    
    return Success
    
    /*
    
    for (i <- 0 until n) {
      if (!starts(i).isBound && demands(i).max > 0) starts(i).callPropagateWhenBoundsChange(this)
      if (!ends(i).isBound && demands(i).max > 0) ends(i).callPropagateWhenBoundsChange(this)
    }
    if (propagate() == Failure) return Failure
    Suspend
    */
  }  
  
  
  
  // variable set to true if the fix-point has not yet finished
  var hasChanged = false

  
  final override def propagate(): CPOutcome = {
    updateBounds()
    do {
      hasChanged = false
      val ok = updateProfile()
      if (!ok) {
        return CPOutcome.Failure // exceed the capa 
      }
      var j = 0
      val m = toConsider.size
      while (j < m) {
        val i = toConsider(j)
        if (!filter(i)) {
          return CPOutcome.Failure
        }
        j += 1
      }
    } while (hasChanged)
    cleanUseless()
    CPOutcome.Suspend
  }   
  

  def filter(i: Int): Boolean = {
    if (dur(i) > 0) {
      
      val sortedPred = precedences(i).sortBy{case(j,w) => starts(j).min}
      
      var currEst = 0
      var slackOnCurrEst = 0
      var currProfIdx = 0
      
      for ((j,w) <- sortedPred) {
        var energyBefore: Int = Math.min (dur(j) - (if (hasManda(j)) (mandaMax(j)-mandaMin(j)-1) else 0),w) * demand(j) // TODO: check if not -1 needed on w
        while (energyBefore > 0) {
          /*
          val energyOnSegment = (profileMax(currProfIdx)-currEst+1)*(capa-profileHeight(currProfIdx))
          if (energyOnSegment <= energyBefore) {
            energyBefore -= energyOnSegment
            currProfIdx += 1
            currEst = 
          }
          */
          
        }
        
        
        
        
        
        
        
        
      }
      
      
      
      
      
      var profIdx = 0 // current index of the profile segment
      val d = demand(i)
      var changed = false
      // ----------------------------
      // left to right
      var newMin = startMin(i)
      profIdx = indexStartMin(i)
      while (profIdx < nProfile && profileMin(profIdx) < math.min(newMin + dur(i),startMax(i))) {
        if (capa - profileHeight(profIdx) < d) {
          newMin = math.min(startMax(i),profileMax(profIdx) + 1) 
        }
        profIdx += 1
      }
      if (newMin > startMin(i)) {
        changed = true
        if (newMin > startMax(i)) return false
        else starts(i).updateMin(newMin)
      }
      // ----------------------------
      // right to left
      var newMax = endMax(i)
      profIdx = indexEndMaxMinusOne(i)
      while (profIdx >= 0 && profileMax(profIdx) >= math.max(newMax - dur(i),endMin(i))) {
        if (capa - profileHeight(profIdx) < d) {
          newMax = math.max(endMin(i),profileMin(profIdx))
        }
        profIdx -= 1
      }
      if (newMax < endMax(i)) {
        changed = true
        if (newMax < endMin(i)) return false
        else ends(i).updateMax(newMax)
      }
      // ----------------------------
      if (changed) updateBound(i,newMin,newMax,dur(i),demand(i))
      hasChanged |= hasManda(i) && changed
    }
    return true
  }
  

  


}
