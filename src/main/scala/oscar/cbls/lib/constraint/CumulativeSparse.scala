package oscar.cbls.constraints.lib.global

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.minmax.MaxArray
import oscar.cbls.lib.invariant.numeric.MinusOffsetPos
import oscar.cbls._

case class CumulativeSparse(start: Array[IntValue], duration: Array[IntValue], amount:Array[IntValue], limit:IntValue) extends Invariant with Constraint with IntNotificationTarget{
  
  
  
  for (v <- start.indices) registerStaticAndDynamicDependency(start(v), v)
  for (v <- duration.indices) registerStaticAndDynamicDependency(duration(v), v)
  for (v <- amount.indices) registerStaticAndDynamicDependency(amount(v), v)
  registerStaticAndDynamicDependency(limit)
  registerConstrainedVariable(limit)
  registerConstrainedVariables(start)
  registerConstrainedVariables(duration)
  registerConstrainedVariables(amount)
  
  finishInitialization()
  
  private val profile = new Profile(start.length,amount.map(_.max).sum,model);
  val heights = profile.profile_height;
  for(v <- heights ){
    v.setDefiningInvariant(this)
  }
  private val over = heights.map(h => MinusOffsetPos(h,limit,0L).asInstanceOf[IntValue])
  private val Violation = MaxArray(over)

  for(v <- start.indices){
    profile.insert(start(v).value, duration(v).value, amount(v).value)
  }
  
  override def violation = Violation
  override def violation(v: Value): IntValue = Violation
  
  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    if (start(index) == v) {
      //start
      profile.remove(OldVal, duration(index).value, amount(index).value)
      profile.insert(NewVal, duration(index).value, amount(index).value)
    } else if (duration(index) == v) {
      //duration
      if (OldVal > NewVal) {
        profile.remove(NewVal + start(index).value, OldVal - NewVal, amount(index).value)
      } else {
        profile.insert(OldVal + start(index).value, NewVal - OldVal, amount(index).value)
      }
    } else {
      if(OldVal > NewVal){
        profile.remove(start(index).value,duration(index).value,OldVal-NewVal)
      }else{
        profile.insert(start(index).value,duration(index).value,NewVal-OldVal)
      }
    }
    //println(violation)
  }
  
  override def checkInternals(c: Checker) {c.check(false, Some("TODO: Implement checkinternal for CumulativeSparse"))}
}

class Profile(n: Long,maxh: Long,model:Store){
  val profile_start = Array.fill(2L*n+2L)(-1L)
  val profile_length = Array.fill(2L*n+2L)(0L)
  val profile_height = Array.fill(2L*n+2L)(new CBLSIntVar(model,0L,0 to maxh,"height"))
  val profile_next = Array.fill(2L*n+2L)(-1L)
  val profile_prev = Array.fill(2L*n+2L)(-1L)
  var first_free = 1L
  val next_free = Array.tabulate(2L*n+2L)(i => i+1L)
  profile_start(0L) = 0L
  profile_length(0L) = Long.MaxValue
  
  def highest_peak(): Long = {
    var cur = 0L
    var max = 0L
    while(cur != -1L){
      max = math.max(max,profile_height(cur).value)
      cur = profile_next(cur)
    }
    println(max)
    max
  }
  def print_profile(check: Boolean = true){
    var s = ""
    var l = ""
    var h = ""
    var cur = 0L
    while(cur != -1L){
      s += profile_start(cur)+"\t"
      l += profile_length(cur)+"\t"
      h = h + profile_height(cur)+"\t"
      cur = profile_next(cur)
    }
    println(s)
    println(l)
    println(h)
    if(check){
	    cur = 0L
	    while(profile_next(cur)!= -1L){
	      if(profile_start(cur)+profile_length(cur)!=profile_start(profile_next(cur))){
	        println("Problem of consistency")
	        System.exit(0L)
	      }
	      cur = profile_next(cur)
	    }
    }
  }
  def remove(s: Long, d: Long, h: Long){
//    println("Before removing "+(s,d,h))
//    print_profile()
    if(h==0L || d==0L) return
    var cur = 0L
    var rd = d//remaining duration
    while(s > profile_start(cur))cur = profile_next(cur)
    assert(s == profile_start(cur))
    if(s!=profile_start(cur)){
      println(s +"\t"+profile_start(cur))
    }
    while(rd > 0L){
//      println(rd+"\t"+profile_start(cur)+"\t"+profile_length(cur))
      if(rd >= profile_length(cur)){
        profile_height(cur) :-= h
        rd -= profile_length(cur)
      }else{
    	//create a new profile
        val new_prof = first_free
        first_free = next_free(first_free)
        profile_length(new_prof) = profile_length(cur) - rd
        profile_height(new_prof) = profile_height(cur)
        profile_start(new_prof) = profile_start(cur) + rd
        profile_length(cur) = rd
        profile_height(cur) :-= h
        profile_next(new_prof) = profile_next(cur)
        if(profile_next(cur)!= -1L)profile_prev(profile_next(cur)) = new_prof
        profile_next(cur) = new_prof
        profile_next(new_prof) = cur
        rd = 0L
      }
      //merge with previous one
      if(profile_prev(cur)!= -1L && profile_height(profile_prev(cur)).newValue == profile_height(cur).newValue){
//        println("HERE")
        val prev = profile_prev(cur)
        profile_length(prev) += profile_length(cur)
        profile_next(prev) = profile_next(cur)
        profile_prev(profile_next(cur)) = prev
        profile_next(cur) = -1L
        profile_prev(cur) = -1L
        profile_start(cur) = -1L
        profile_length(cur) = 0L
        profile_height(cur) := 0L
        next_free(cur) = first_free
        first_free = cur
        cur = prev
      }
//      println(rd + "\t" + cur + "\t" + profile_next(cur))
      cur = profile_next(cur)
    }
    //merge with previous one
      if(profile_prev(cur)!= -1L && profile_height(profile_prev(cur)).newValue == profile_height(cur).newValue){
        val prev = profile_prev(cur)
        profile_length(prev) += profile_length(cur)
        profile_next(prev) = profile_next(cur)
        if(profile_next(cur)!= -1L)profile_prev(profile_next(cur)) = prev
        profile_next(cur) = -1L
        profile_prev(cur) = -1L
        profile_start(cur) = -1L
        profile_length(cur) = 0L
        profile_height(cur) := 0L
        next_free(cur) = first_free
        first_free = cur
        cur = prev
      }
//    println("After removing "+(s,d,h))
//    print_profile()
  }
  
  def insert(s: Long, d: Long, h: Long): Long = {
    println("Before inserting "+(s,d,h))
    print_profile()
    if(h==0L || d==0L) return -1L
    var cur = 0L
    var rd = d//remaining duration
    var cs = s//current start
    
    while(profile_next(cur)!= -1L && cs >= profile_start(profile_next(cur)) ) cur = profile_next(cur)
    
    
    //var next = profile_next(cur)
    var next_h = profile_height(cur).newValue
    val res = if(profile_start(cur) < cs){first_free}else{cur}
    if(profile_start(cur) < cs){
      //new profile necessary
      val new_prof = first_free
      first_free = next_free(first_free)
      //1. cut into two parts with same profile
      profile_start(new_prof) = cs
      profile_length(new_prof) = profile_start(cur)+profile_length(cur)-cs
      profile_height(new_prof) := profile_height(cur).newValue
      
      profile_length(cur) = profile_start(new_prof) - profile_start(cur)
      
      profile_next(new_prof) = profile_next(cur)
      if(profile_next(cur) != -1L)profile_prev(profile_next(cur)) = new_prof
      profile_next(cur) = new_prof
      profile_prev(new_prof) = cur
      
      cur = new_prof
    }
    println("after initial")
    print_profile(false)
    while(rd > 0L){
      
      if(cs != profile_start(cur)){
        println("PROBLEM HERE")
        println(profile_start(0L)  + "\t"+ cur + "\t"+ cs + "\t"+ rd  + "\t"+ profile_start(cur) + "\t"+ d  + "\t"+ s  + "\t"+ h)
        print_profile()
      }
      next_h = profile_height(cur).newValue
      
      
      profile_height(cur) :+= h
      profile_length(cur) = math.min(profile_length(cur),rd) 
      cs = profile_start(cur) + profile_length(cur)
      rd = rd - profile_length(cur)
      
      if(rd > 0L) cur = profile_next(cur)
    }
    
    //attach the last(cur) profile to a new next one.
    if(profile_next(cur)== -1L){
      val new_prof = first_free
      first_free = next_free(first_free)
      profile_start(new_prof) = cs
      profile_length(new_prof) = Long.MaxValue - profile_start(new_prof)
      profile_height(new_prof) := next_h
      profile_next(new_prof) = profile_next(cur)
      profile_next(cur) = new_prof
      profile_prev(new_prof) = cur
    }else if(cs < profile_start(profile_next(cur))){
      val new_prof = first_free
      first_free = next_free(first_free)
      profile_start(new_prof) = cs
      profile_length(new_prof) = profile_start(profile_next(cur)) - cs
      profile_height(new_prof) := next_h
      profile_next(new_prof) = profile_next(cur)
      profile_prev(profile_next(cur)) = new_prof
      profile_next(cur) = new_prof
      profile_prev(new_prof) = cur
    }
    println("After inserting "+(s,d,h))
    print_profile()
    res
  }
}