package oscar.cp.premulative

import oscar.cp.core._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import java.lang.Math._
import oscar.algo.SortUtils
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversibleSparseSet

/**
 * @author Pierre Schaus
 */

class CumulativeLinearWithLags(startsArg: Array[CPIntVar], durationsArg: Array[CPIntVar], endsArg: Array[CPIntVar], demandsArg: Array[CPIntVar], resourcesArg: Array[CPIntVar], capaArg: CPIntVar, id: Int,delay: Array[Array[Int]]) extends Constraint(startsArg.head.store, "TimeTable") {

  val args = Array.tabulate(startsArg.size)(i => i).filter(i => demandsArg(i).max > 0 && durationsArg(i).max > 0 && resourcesArg(i).hasValue(id))
  private[this] val starts = args.map(startsArg(_))
  private[this] val ends = args.map(endsArg(_))
  private[this] val durations = args.map(durationsArg(_))
  private[this] val demands = args.map(demandsArg(_))
  
  val pairs = args.zipWithIndex.map(i => args.zipWithIndex.map(j => (i._2,j._2,delay(i._1)(j._1),delay(j._1)(i._1)))).flatten[Tuple4[Int,Int,Int,Int]].filter{case (i,j,d,e) => (i < j && d > Int.MinValue && e > Int.MinValue)}
  val dist = args.map(i => args.map(j => delay(i)(j)))
  //println(pairs.length)
  priorityL2 = 0
  idempotent = true

  private[this] val n = starts.size

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
    profilePoint(np) = Int.MinValue+1
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
    profilePoint(np) = Int.MaxValue-1
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
    profileMax(nProfile-1) = Int.MaxValue-1
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
    for (i <- 0 until n) {
      if (!starts(i).isBound && demands(i).max > 0) starts(i).callPropagateWhenBoundsChange(this)
      if (!ends(i).isBound && demands(i).max > 0) ends(i).callPropagateWhenBoundsChange(this)
    }
    if (propagate() == Failure) return Failure
    Suspend
  }  
  
  
  // variable set to true if the fix-point has not yet finished
  var hasChanged = false

  
  final override def propagate(): CPOutcome = {
    println("BEFORE:\t"+starts.mkString("\t"))
    updateBounds()
    do {
      hasChanged = false
      val ok = updateProfile()
      println(profileMin.mkString(", "))
      println(profileHeight.mkString(", "))
      if (!ok) {
        return CPOutcome.Failure // exceed the capa 
      }
      /*
      var z = 0
      val w = pairs.size
      while(z < w){
        val (i,j,dij,dji) = pairs(z)
        if(!filter(i,j,dij,dji)){
//          println("FAIL:\t"+starts.mkString("\t"))
          return CPOutcome.Failure
        }
        if(!filter(j,i,dji,dij)){
//          println("FAIL:\t"+starts.mkString("\t"))
          return CPOutcome.Failure
        }
        //System.exit(0)
        z +=1
      }*/
      val orderedTasks = toConsider.toArray
      SortUtils.mergeSort(orderedTasks, startMin, 0, orderedTasks.length,runs1,aux1)
      var z = 0
      while(z < orderedTasks.length){
        var y = z+1
        while(y < orderedTasks.length && startMin(orderedTasks(y)) < endMin(orderedTasks(z))){//if they overlap in time. If one does not, the next won't either.
          if(!hasManda(orderedTasks(z)) && !hasManda(orderedTasks(y))){//We should do something better here but for now we are conservative and avoid any mandatory part.
            val res1 = filter(orderedTasks(z),orderedTasks(y))
            val res2 = filter(orderedTasks(y),orderedTasks(z))
            if(!res1 || !res2){
              return CPOutcome.Failure
            }
          }
          y+=1
        }
        z+=1
      }
      
      var j = 0
      val m = toConsider.size
      while (j < m) {
        val i = toConsider(j)
        if (!filter(i)) {
          println("FAIL here?: "+starts.mkString("\t"))
          return CPOutcome.Failure
        }
        j += 1
      }
    } while (hasChanged)
    cleanUseless()
    println("AFTER:\t"+starts.mkString("\t"))
    System.exit(0)
    CPOutcome.Suspend
  }   
  /**
   * Filter the domain of i with respect to its potential conflict with j
   */
  def filter(i: Int, j: Int): Boolean = {
    //1. Test whether there is a conflict!
    //Build the intersection of the tasks
    val intersection = (math.max(startMin(i),startMin(j)),math.min(endMin(i),endMin(j)))
    //Get the first profile
    var p = math.max(indexStartMin(i),indexStartMin(j))
    //Test if any profile creates a conflict
    var cp = -1
    while(profileMin(p) < intersection._2){
      if(demand(i)+demand(j)+profileHeight(p) > capa) cp = p
      p+=1
    }
    //There is no conflict, we quit
    if(cp== -1) return true
    //Otherwise, cp is the last conflicting profile section. We will try to avoid that one.
    val endofcp = math.min(profileMax(cp)+1,math.min(endMin(i),endMin(j)))
    println(cp+" "+i+" "+j+" "+endofcp)
    
    //2. Test whether j can be pushed after i without any conflict
    //first case: the task can be scheduled at its latest without overlapping and without violating the precedence
    if(startMax(j)>=endMin(i) && startMin(i) >= startMax(j)+dist(j)(i)) return true
    //second case: j can be moved such that start(j) >= min(endMin(i),endMin(j)) && start(j)+dist(j)(i) =< startMin(i)
    //There is min above because we want to schedule after the conflict.
    //Do a sweep
    val canBeMoved = canBeMovedRight(j,math.min(endMin(i),endMin(j)),startMin(i)-dist(j)(i))
    if(canBeMoved) return true
    //2. if task j cannot be moved, then we need to move task i so as to resolve the *current* conflict
    true
  }
  
  //can task j be scheduled somewhere between min and max, given the current schedule.
  def canBeMovedRight(j: Int, min: Int, max: Int): Boolean  = {
    if(min > max){
      println("YEP")
      return false
    }
    true
  }
  
  def pushLeft(i: Int, ti0: Int, si0: Int, dji: Int, cj: Int, dj: Int, j: Int, canAdd: Boolean, dij: Int, tj: Int): (Int, Int) = {
    var ti = ti0
    var oldti = ti;
    var si = si0
    var changed = false;
    while (si < nProfile && math.max(profileMin(si),ti) < math.min(ti + dur(i),startMax(i))) {
      changed = false
      //To add, you must be in the part of the task that must overlap, and that is still under the other task.
      //TODO: Check also that we are not in the mandatory part of j
      val intersection = (/*math.min(*/math.max(ti-dji,math.max(ti,tj))/*,math.max(ti,math.max(ti+dij,tj)))*/,math.min(ti+dur(i),cj))
      val interprofile = (math.max(intersection._1,profileMin(si)),math.min(intersection._2,profileMax(si)+1))
      val manda = (mandaMin(j),mandaMax(j)+1)
      val internotempty = interprofile._1 < interprofile._2
      val mustAdd = internotempty && (!hasManda(j) || (interprofile._1 < manda._1 || interprofile._2 > manda._2))
//      if(i==5 && j ==1){
//        println(intersection)
//        println(interprofile)
//        println(manda)
//        println(internotempty)
//        println(mustAdd)
//        println(canAdd)
//      }
      val add = if (canAdd && mustAdd) //!hasManda(j) && profileMax(si) > math.max(ti - dji,ti) && profileMin(si) < math.min(cj,ti + dur(i)) && math.max(ti - dji,ti) < math.min(cj,ti+dur(i)))
          dj else 0
      if(add > 0) println("add " + add  +"\t"+ i +"\t"+ ti +"\t"+ dji +"\t"+ cj +"\t"+ si +"\t"+ dur(i) +"\t"+ profileMax(si) +"\t"+ profileMin(si) +"\t"+ profileHeight(si))
//      else println("no add \t"+ i +"\t"+ ti +"\t"+ dji +"\t"+ cj +"\t"+ si +"\t"+ dur(i) +"\t"+ profileMax(si) +"\t"+ profileMin(si) +"\t"+ profileHeight(si))
        if (capa - profileHeight(si) < demand(i) + add) {
          //why do we need to min with startMax(i)?
          if(ti < cj){
            ti = math.min(cj,
                //math.min(startMax(i),
                    profileMax(si) + 1)//)
          }else{
            ti = //math.min(startMax(i),
              profileMax(si) + 1//)
          }
          println(ti)
          val lengthinter = interprofile._2 - interprofile._1
          if(add>0){
            //can only move a bit, and need to move j as well!! But in which cases?
            ti = math.min(ti,oldti+lengthinter)
          }
          println(ti)
          changed = true
          //TODO: need to move only after the task if problem due to added capacity!
        }
        si += 1
        if(changed && ti > profileMin(si)){//because we might just go over the edge of the other task without moving
          si-=1
          //println(ti+" "+si+" "+cj)
        }
        oldti=ti
      }
    //si - 1 because we did not check si yet, max because we might have not entered the loop
    (ti,math.max(si0,si-1))
  }
  def filter(i: Int, j: Int, dij: Int, dji: Int):  Boolean = {
    if(dur(i) > 0 && dur(j) > 0){
//    println("while:\t"+starts.mkString("\t"))
      println("filter  for "+i+"\t"+dur(i)+"\t"+demand(i)+"\t"+dij+"\tand\t"+j+"\t"+dur(j)+"\t"+demand(j)+"\t"+dji)
      println("with domains"+ startMin(i)+".."+startMax(i)+" and "+ startMin(j)+".."+startMax(j))

      var profIdxi = 0 // current index of the profile segment
      var profIdxj = 0 // current index of the profile segment
      val di = demand(i)
      val dj = demand(j)
      var changedi = false
      var changedj = false
      // ----------------------------
      // left to right
      var newMini = startMin(i)
      profIdxi = indexStartMin(i)
      var newMinj = startMin(j)
      profIdxj = indexStartMin(j)
      println("init\t"+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji+"\t"+dur(i)+" "+dur(j))
      //move Mini to be cumulative-feasible
      val tmp = pushLeft(i,newMini,profIdxi,dji,newMinj+dur(j),demand(j),j, dji >= 0,dij, newMinj)
      newMini = tmp._1
      profIdxi = tmp._2
      println("placed i "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji+"\t"+dur(i)+" "+dur(j))
      //move Minj to respect the precedence
      newMinj = math.max(newMinj,newMini+dij)
      while(profIdxj < nProfile && profileMax(profIdxj) < newMinj){
        profIdxj+=1
      }
      println("moved j "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji+"\t"+dur(i)+" "+dur(j))
       
      //println(newMinj)
      //Move Minj to be cumulative-feasible
      val tmp2 = pushLeft(j,newMinj,profIdxj,dij,newMini+dur(i),demand(i),i, true,dji, newMini)
      newMinj = tmp2._1
      profIdxj = tmp2._2
      println("placed j "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji+"\t"+dur(i)+" "+dur(j))
      while(newMini < newMinj+dji){
//        println("A "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji+"\t"+dur(i)+" "+dur(j))
       // System.exit(0)
        //move Mini to respect the precedence
        newMini = newMinj+dji
        if (newMini > startMax(i)) return false
        while(profIdxi < nProfile && profileMax(profIdxi) < newMini){
          profIdxi +=1
        }
        //println("B "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji)
        //Move Mini to be cumulative-feasible
        val tmp = pushLeft(i,newMini,profIdxi,dji,newMinj+dur(j),demand(j),j, dji>=0,dij,newMinj)
        newMini = tmp._1
        profIdxi = tmp._2
        //println("C "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji)
        //move Minj to respect the precedence
        newMinj = math.max(newMinj,newMini+dij)
        if (newMinj > startMax(j)) return false
        while(profIdxj < nProfile && profileMax(profIdxj) < newMinj){
          profIdxj+=1
        }
        //println("D "+profIdxi+"\t"+profIdxj+"\t"+newMini+"\t"+newMinj+"\t"+dij+"\t"+dji)
        //Move Minj to be cumulative-feasible
        val tmp2 = pushLeft(j,newMinj,profIdxj,dij,newMini+dur(i),demand(i),i, true,dji,newMini)
        newMinj = tmp2._1
        profIdxj = tmp2._2
      }
      //println("c")
      if (newMini > startMin(i)) {
        changedi = true
        if (newMini > startMax(i)) return false
        else starts(i).updateMin(newMini)
      }
      /*if (newMinj > startMin(j)) {
        changedj = true
        if (newMinj > startMax(j)) return false
        else starts(j).updateMin(newMinj)
      }*/
      // ----------------------------
      // right to left
      var newMaxi = endMax(i)
      var newMaxj = endMax(j)
//      profIdx = indexEndMaxMinusOne(i)
//      while (profIdx >= 0 && profileMax(profIdx) >= math.max(newMax - dur(i),endMin(i))) {
//        if (capa - profileHeight(profIdx) < d) {
//          newMax = math.max(endMin(i),profileMin(profIdx))
//        }
//        profIdx -= 1
//      }
//      if (newMax < endMax(i)) {
//        changed = true
//        if (newMax < endMin(i)) return false
//        else ends(i).updateMax(newMax)
//      }
      // ----------------------------
      if (changedi) updateBound(i,newMini,newMaxi,dur(i),demand(i))
      hasChanged |= hasManda(i) && changedi
      /*if (changedj) updateBound(j,newMinj,newMaxj,dur(j),demand(j))
      hasChanged |= hasManda(j) && changedj*/
      //if(changedi||changedj)System.exit(0)
      if(changedi){
        println("new bound for "+i+" = "+newMini)
      }
    }
    true
  }
  
  def filter(i: Int): Boolean = {
    if (dur(i) > 0) {
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
