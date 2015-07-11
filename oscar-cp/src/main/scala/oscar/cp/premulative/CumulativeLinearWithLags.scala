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

class CumulativeLinearWithLags(startsArg: Array[CPIntVar], durationsArg: Array[CPIntVar], endsArg: Array[CPIntVar], demandsArg: Array[CPIntVar], resourcesArg: Array[CPIntVar], capaArg: CPIntVar, id: Int,delay: Array[Array[Int]],pruneest: Boolean, prunelst: Boolean) extends Constraint(startsArg.head.store, "TimeTable") {

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
//    println("BEFORE:\t"+starts.mkString("\t"))
    updateBounds()
    do {
      hasChanged = false
      val ok = updateProfile()
//      println(profileMin.mkString(", "))
//      println(profileHeight.mkString(", "))
      if (!ok) {
        return CPOutcome.Failure // exceed the capa 
      }
      
      if(pruneest){//LTR 
        //TODO: Update orderedTasks and toConsider in an incremental way
        val orderedTasks = toConsider.toArray
        SortUtils.mergeSort(orderedTasks, startMin, 0, orderedTasks.length,runs1,aux1)
        var z = 0
        while(z < orderedTasks.length){
          if(!hasManda(orderedTasks(z))){
            var y = z+1
            while(y < orderedTasks.length && startMin(orderedTasks(y)) < endMin(orderedTasks(z))){//if they overlap in time. If one does not, the next won't either.
              if(!hasManda(orderedTasks(z)) && !hasManda(orderedTasks(y))){//We should do something better here but for now we are conservative and avoid any mandatory part.
                val res1 = filterESTBoth(orderedTasks(z),orderedTasks(y))
                //val res2 = filterEST(orderedTasks(y),orderedTasks(z))
                if(!res1 /*|| !res2*/){
                  return CPOutcome.Failure
                }
              }
              y+=1
            }
          }
          z+=1
        }
      }
      if(prunelst){ //RTL
        val orderedTasks = toConsider.toArray
        SortUtils.mergeSort(orderedTasks, endMax, 0, orderedTasks.length,runs1,aux1)
        var z = orderedTasks.length-1
        while(z >=0){
          if(!hasManda(orderedTasks(z))){
            var y = z-1
            while(y >= 0 && endMax(orderedTasks(y)) > startMax(orderedTasks(z))){//if they overlap in time. If one does not, the next won't either.
              if(!hasManda(orderedTasks(z)) && !hasManda(orderedTasks(y))){//We should do something better here but for now we are conservative and avoid any mandatory part.
                val res1 = filterLSTBoth(orderedTasks(z),orderedTasks(y))
                //val res2 = filterLST(orderedTasks(y),orderedTasks(z))
                if(!res1 /*|| !res2*/){
                  return CPOutcome.Failure
                }
              }
              y-=1
            }
          }
          z-=1
        }
      }
      /*
      var j = 0
      val m = toConsider.size
      while (j < m) {
        val i = toConsider(j)
        if (!filter(i)) {
          println("FAIL here?: "+starts.mkString("\t"))
          return CPOutcome.Failure
        }
        j += 1
      }*/
    } while (hasChanged)
    cleanUseless()
//    println("AFTER:\t"+starts.mkString("\t"))
    //System.exit(0)
    CPOutcome.Suspend
  }   
  /**
   * Filter the domain of i with respect to its potential conflict with j at their est
   */
  def filterEST(i: Int, j: Int): Boolean = {
    //1. Test whether there is a conflict!
    //Build the intersection of the tasks
    val intersection = (math.max(startMin(i),startMin(j)),math.min(endMin(i),endMin(j)))
    //Get the first profile
    var p = math.max(indexStartMin(i),indexStartMin(j))
    //Test if any profile creates a conflict
    var cp = -1
    val demandi = demand(i)
    val demandj = demand(j)
    while(p < nProfile && profileMin(p) < intersection._2){
      if(demandi+demandj+profileHeight(p) > capa) cp = p
      p+=1
    }
    //There is no conflict, we quit
    if(cp== -1) return true
    //Otherwise, cp is the last conflicting profile section. We will try to avoid that one.
    val endofcp = math.min(if(cp+1 < nProfile) profileMin(cp+1) else Int.MaxValue, intersection._2)
    val startofcp = math.max(profileMin(cp),intersection._1)
//    if(startMin(i)==48) println((startofcp,endofcp))
    //println(cp+" "+i+" "+j+" "+endofcp)
    
    
    //Until here it is common for i=a,j=b and for i=b,j=a, so we should factor the code.
    
    //2. Test whether j can be pushed after endofcp without any conflict
    val distji = dist(j)(i)
    //first case: the task can be scheduled at its latest without overlapping and without violating the precedence
    if(startMax(j)>=endofcp && startMin(i) >= startMax(j)+distji) return true
    //second case: j can be moved such that start(j) >= endofcp && start(j)+dist(j)(i) =< startMin(i)
    //Do a sweep
    val canBeMoved = canBeMovedRight(j,endofcp,startMin(i)-distji,cp)
    if(canBeMoved) return true
    //2. if task j cannot be moved, then we need to move task i so as to resolve the *current* conflict
    val lengthofcp = endofcp-startofcp
    //TODO: Check that the length of the move is both correct and optimal (note that moving by 1 is correct but not optimal)
    val move = if(lengthofcp+dist(j)(i) > 0)math.min(lengthofcp,lengthofcp+distji) else lengthofcp//?
    //if(startMin(i)==48) 
//      println("move by " + move+" to "+(startMin(i)+move)+" vs "+lengthofcp)
    if (startMin(i)+move > startMax(i)) return false
    else starts(i).updateMin(startMin(i)+move)
    return true
  }
  
  //can start of task j be scheduled somewhere between min and max, given the current schedule.
  def canBeMovedRight(j: Int, min: Int, max: Int, firstp: Int): Boolean  = {
    if(min > max){
      return false
    }
    return true
    
    //Do a sweep to make sure it can be placed without conflict!
    var p = firstp
    var t = min
    val durj = dur(j)
    val demandj = demand(j)
    while(p < nProfile && profileMin(p) < math.min(t+durj,max+durj)){
      if(profileHeight(p) + demandj > capa){
        t = profileMin(p+1)
      }
      p+=1
    }
    if(t> max) return false
    else true
  }
  

  
  /**
   * Filter the domain of i with respect to its potential conflict with j at their est
   */
  def filterESTBoth(i: Int, j: Int): Boolean = {
    //1. Test whether there is a conflict!
    //Build the intersection of the tasks
    val intersection = (math.max(startMin(i),startMin(j)),math.min(endMin(i),endMin(j)))
    //Get the first profile
    var p = math.max(indexStartMin(i),indexStartMin(j))
    //Test if any profile creates a conflict
    var cp = -1
    val demandi = demand(i)
    val demandj = demand(j)
    while(p < nProfile && profileMin(p) < intersection._2){
      if(demandi+demandj+profileHeight(p) > capa) cp = p
      p+=1
    }
    //There is no conflict, we quit
    if(cp== -1) return true
    //Otherwise, cp is the last conflicting profile section. We will try to avoid that one.
    val endofcp = math.min(if(cp+1 < nProfile) profileMin(cp+1) else Int.MaxValue, intersection._2)
    val startofcp = math.max(profileMin(cp),intersection._1)
    //val lengthofcp = endofcp-startofcp
        
    //return true
    
    //Until here it is common for i=a,j=b and for i=b,j=a, so we should factor the code.
    //2. Test whether j can be pushed after endofcp without any conflict
    val distji = dist(j)(i)
    //first case: the task can be scheduled at its latest without overlapping and without violating the precedence
    //second case: j can be moved such that start(j) >= endofcp && start(j)+dist(j)(i) =< startMin(i)
    if(!((startMax(j)>=endofcp && startMin(i) >= startMax(j)+distji)) && !canBeMovedRight(j,endofcp,startMin(i)-distji,cp)){
      //3. if task j cannot be moved, then we need to move task i so as to resolve the *current* conflict
      //TODO: Check that the length of the move is both correct and optimal (note that moving by 1 is correct but not optimal)
      val move = endofcp - (if(startMin(i)-distji < endofcp)math.max(startofcp,startMin(i)-distji) else startofcp)//?
      if (startMin(i)+move > startMax(i)) return false
      else starts(i).updateMin(startMin(i)+move)
    }
    
    //2. Test whether i can be pushed after endofcp without any conflict
    val distij = dist(i)(j)
    //first case: the task can be scheduled at its latest without overlapping and without violating the precedence
    //second case: i can be moved such that start(i) >= endofcp && start(i)+dist(i)(j) =< startMin(j)
    if((startMax(i)<endofcp || startMin(j) < startMax(i)+distij) && !canBeMovedRight(i,endofcp,startMin(j)-distij,cp)){
      //3. if task j cannot be moved, then we need to move task i so as to resolve the *current* conflict
      //TODO: Check that the length of the move is both correct and optimal (note that moving by 1 is correct but not optimal)
      val move = endofcp - (if(startMin(j)-distij < endofcp)math.max(startofcp,startMin(j)-distij) else startofcp)
      if (startMin(j)+move > startMax(j)) return false
      else starts(j).updateMin(startMin(j)+move)
    }
    return true
  }

  
    /**
   * Filter the domain of i with respect to its potential conflict with j at their lst
   */
  def filterLST(i: Int, j: Int): Boolean = {
    //println("filter "+i+ " using "+j)
//    if(demand(i) + demand(j) > capa){
//      println("should filter "+i+ " using "+j)
//    }
    //1. Test whether there is a conflict!
    //Build the intersection of the tasks
    val intersection = (math.max(startMax(i),startMax(j))-1,math.min(endMax(i),endMax(j))-1)
    //Get the first profile
    var p = math.max(indexEndMaxMinusOne(i),indexEndMaxMinusOne(j))
    //Test if any profile creates a conflict
    var cp = -1
    val demandi = demand(i)
    val demandj = demand(j)
    while(p > -1 && profileMax(p) > intersection._1){
      if(demandi+demandj+profileHeight(p) > capa) cp = p
      p-=1
    }
    //There is no conflict, we quit
    if(cp== -1) return true
    //Otherwise, cp is the first conflicting profile section. We will try to avoid that one.
    val endofcp = math.max(if(cp-1 > -1) profileMax(cp-1) else Int.MinValue, intersection._1)
    val startofcp = math.min(profileMax(cp),intersection._2)
//    if(startMin(i)==48) println((startofcp,endofcp))
    //println(cp+" "+i+" "+j+" "+endofcp)
    
    
    //Until here it is common for i=a,j=b and for i=b,j=a, so we should factor the code.
    
    
    //2. Test whether j can be pushed before endofcp without any conflict
    //ATTENTION: The distances are with respect to the start times, not the end times
    val distij = dist(i)(j)
    //first case: the task can be scheduled at its earliest without overlapping and without violating the precedence
    //if(startMax(j)>=endofcp && startMin(i) >= startMax(j)+distji) return true
    if(endMin(j)-1<=endofcp && startMax(i) <= startMin(j)-distij) return true
    //println("HERE")
    //second case: j can be moved such that start(j) >= endofcp && start(j)+dist(j)(i) =< startMin(i)
    //Do a sweep
    val canBeMoved = canBeMovedLeft(j,endofcp,startMax(i)-1+distij+dur(j),cp)
    if(canBeMoved) return true
    //2. if task j cannot be moved, then we need to move task i so as to resolve the *current* conflict
    //negative
    val lengthofcp = endofcp-startofcp
    //TODO: Check that the length of the move is both correct and optimal (note that moving by 1 is correct but not optimal)
    val move = if(lengthofcp-distij < 0)math.max(lengthofcp,lengthofcp-distij) else lengthofcp//?
    //if(startMin(i)==48) 
//      println("move by " + move+" to "+(startMin(i)+move)+" vs "+lengthofcp)
    if (startMax(i)+move < startMin(i)) return false
    else starts(i).updateMax(startMax(i)+move)
    return true
  }
  
  
  
    /**
   * Filter the domain of i with respect to its potential conflict with j at their lst
   */
  def filterLSTBoth(i: Int, j: Int): Boolean = {
    //println("filter "+i+ " using "+j)
//    if(demand(i) + demand(j) > capa){
//      println("should filter "+i+ " using "+j)
//    }
    //1. Test whether there is a conflict!
    //Build the intersection of the tasks
    val intersection = (math.max(startMax(i),startMax(j))-1,math.min(endMax(i),endMax(j))-1)
    //Get the first profile
    var p = math.max(indexEndMaxMinusOne(i),indexEndMaxMinusOne(j))
    //Test if any profile creates a conflict
    var cp = -1
    val demandi = demand(i)
    val demandj = demand(j)
    while(p > -1 && profileMax(p) > intersection._1){
      if(demandi+demandj+profileHeight(p) > capa) cp = p
      p-=1
    }
    //There is no conflict, we quit
    if(cp== -1) return true
    //Otherwise, cp is the first conflicting profile section. We will try to avoid that one.
    val endofcp = math.max(if(cp-1 > -1) profileMax(cp-1) else Int.MinValue, intersection._1)
    val startofcp = math.min(profileMax(cp),intersection._2)
      //negative value
    val lengthofcp = endofcp-startofcp
    
    //Until here it is common for i=a,j=b and for i=b,j=a, so we should factor the code.
    //return true
    
    //2. Test whether j can be pushed before endofcp without any conflict
    //ATTENTION: The distances are with respect to the start times, not the end times
    val distij = dist(i)(j)
    //first case: the task can be scheduled at its earliest without overlapping and without violating the precedence
    //second case: j can be moved such that start(j) >= endofcp && start(j)+dist(j)(i) =< startMin(i)
    if(!(endMin(j)-1<=endofcp && startMax(i) <= startMin(j)-distij) && !canBeMovedLeft(j,endofcp,startMax(i)-1+distij+dur(j),cp)){
      //3. if task j cannot be moved, then we need to move task i so as to resolve the *current* conflict
      //TODO: Check that the length of the move is both correct and optimal (note that moving by 1 is correct but not optimal)
      //if(lengthofcp+dist(i)(j) > 0)math.min(lengthofcp,lengthofcp+distij) else lengthofcp
      //val move = endofcp - (if(startMin(i)-distji < endofcp)math.max(startofcp,startMin(i)-distji) else startofcp)//?
      val move = endofcp - (if(startMax(i)+distij > endofcp)math.min(startofcp,startMax(i)+distij) else startofcp)//?
      
//      (if(lengthofcp-distij < 0)math.max(lengthofcp,lengthofcp-distij) else endofcp)
      if (startMax(i)+move < startMin(i)) return false
      else starts(i).updateMax(startMax(i)+move)
    }
    
    //2. Test whether i can be pushed before endofcp without any conflict
    //ATTENTION: The distances are with respect to the start times, not the end times
    val distji = dist(j)(i)
    //first case: the task can be scheduled at its earliest without overlapping and without violating the precedence
    //second case: j can be moved such that start(j) >= endofcp && start(j)+dist(j)(i) =< startMin(i)
    if(!(endMin(i)-1<=endofcp && startMax(j) <= startMin(i)-distji) && !canBeMovedLeft(i,endofcp,startMax(j)-1+distji+dur(i),cp)){
      //3. if task i cannot be moved, then we need to move task j so as to resolve the *current* conflict
      //TODO: Check that the length of the move is both correct and optimal (note that moving by 1 is correct but not optimal)
      val move = endofcp - (if(startMax(j)+distji > endofcp)math.min(startofcp,startMax(j)+distji) else startofcp)//?
      if (startMax(j)+move < startMin(j)) return false
      else starts(j).updateMax(startMax(j)+move)
    }
    return true
  }
  
  
  //can task j be scheduled somewhere between min and max, given the current schedule.
  def canBeMovedLeft(j: Int, max: Int, min: Int, firstp: Int): Boolean  = {
//    if(startMin(j)==46){
//      println(j + " "+min+ " "+max+ " "+profileHeight(firstp))
//    }
    if(max < min){
    //  println("YEP")
      return false
    }
  //  println("NO")
    //Do a sweep to make sure it can be placed without conflict!
    var p = firstp
    var t = max
    val durj = dur(j)
    val demandj = demand(j)
    while(p > -1 && profileMax(p) > math.max(t-durj,min-durj)){
    //  println(" "+p+" "+profileMin(p)+" "+(t+dur(j))+" "+max+" "+profileHeight(p))
      if(profileHeight(p) + demandj > capa){
        t = profileMax(p-1)
      }
      p-=1
    }
//    println(t+"\t"+max)
    if(t< min) return false
    else true
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
