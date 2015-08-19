package oscar.des.flow.lib

import oscar.des.engine.Model

abstract class Time{
  def time:Int
}
//This file is about thing we want to measure on the factory process

//Variables have values at all time.
abstract class BoolExpr{
  def value:Boolean
}
abstract class IntExpr{
  def value:Int
}

//probe on simulation elements
class Empty(s:Storage) extends BoolExpr{
  override def value: Boolean = s.contentSize == 0
}
class Productive(p:ActivableProcess) extends BoolExpr{
  override def value: Boolean = p.isRunning
}
class Content(s:Storage) extends IntExpr{
  override def value: Int = s.contentSize
}

//logical properties
//boolean is whet it means: a boolean value at each state. there is no notion of event there; they are like fluents.
//we only consider temporal operators of the past, easy to evaluate
class Not(f:BoolExpr) extends BoolExpr{
  override def value: Boolean = !f.value
}
class And(f:BoolExpr, g:BoolExpr) extends BoolExpr{
  //We cannot be lazy here because all our expression might need to be updated due to side effect.
  override def value: Boolean = if(f.value) g.value else false
}
class Or(f:BoolExpr, g:BoolExpr) extends BoolExpr{
  override def value: Boolean = if(f.value) true else g.value
}

/**has always ben on each query, so you have to make a qery at each time step*/
class HasAlwaysBeen(f:BoolExpr) extends BoolExpr{
  var hasAlwaysBeen = f.value

  override def value: Boolean = if(hasAlwaysBeen) {
    hasAlwaysBeen &= f.value
    hasAlwaysBeen
  }else false
}

class HasBeen(f:BoolExpr) extends BoolExpr{
  var hasBeen = f.value

  override def value: Boolean = if(hasBeen) true else{
    hasBeen |= f.value
    hasBeen
  }
}

/**
 * true if exists s <= t where b AND for each u in [s,t] a holds
 * @param a
 * @param b
 */
class Since(a:BoolExpr,b:BoolExpr) extends BoolExpr {
  var previousValue = a.value && b.value

  override def value: Boolean = {
    if(previousValue){
      if(a.value) true
      else{
        previousValue = false
        false
      }
    }else{
      if (a.value && b.value){
        previousValue = true
        true
      }else false
    }
  }
}

class BecomesTrue(p:BoolExpr) extends BoolExpr{
  var previousValue = p.value

  override def value: Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    !oldPreviousValue & previousValue
  }
}

class BecomesFalse(p:BoolExpr) extends BoolExpr{
  var previousValue = p.value

  override def value: Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    oldPreviousValue & !previousValue
  }
}
class Changes(p:BoolExpr) extends BoolExpr{
  var previousValue = p.value

  override def value: Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    oldPreviousValue != previousValue
  }
}

//variables always have a value.
class CumulatedDuration(b:BoolExpr, t:Time) extends IntExpr{
  var acc:Int = 0
  var wasTrue = b.value
  var previousTime = t.time

  override def value: Int = {
    if(wasTrue){
      if(b.value){
        val now = t.time
        acc += (now - previousTime)
        previousTime = now
      }else{
        wasTrue = false
      }
    }else{
      if(b.value){
        wasTrue = true
        previousTime = t.time
      }
    }
    acc
  }
}

class Mult(a:IntExpr,b:IntExpr) extends IntExpr{
  override def value: Int = a.value * b.value
}
class Plus(a:IntExpr,b:IntExpr) extends IntExpr{
  override def value: Int = a.value + b.value
}
//triangles
class PonderateWithDuration(s:IntExpr,t:Time) extends IntExpr{
  var acc = 0
  var prevTime = t.time
  var prevValue = s.value
  override def value: Int = {
    val now = t.time
    var nowValue = s.value
    acc += (now - prevTime) * ((nowValue + prevValue)/2)
    prevTime = now
    prevValue = nowValue
    acc
  }
}

//relational operators to get back to Propositions
class G(a:IntExpr,b:IntExpr) extends BoolExpr {
  override def value: Boolean = a.value > b.value
}
class GE(a:IntExpr,b:IntExpr) extends BoolExpr{
  override def value: Boolean = a.value >= b.value
}
class LE(a:IntExpr,b:IntExpr) extends BoolExpr{
  override def value: Boolean = a.value <= b.value
}
class EQ(a:IntExpr,b:IntExpr) extends BoolExpr{
  override def value: Boolean = a.value == b.value
}
class NEQ(a:IntExpr,b:IntExpr) extends BoolExpr{
  override def value: Boolean = a.value != b.value
}

//To estimate over different runs
//how to find names that are obviously statistics over different runs

class Statistics
//this only considers the latest valuee of e; at the end of the simulation run, and performs an average over several runs
class Mean(e:IntExpr) extends Statistics
class Variance(e:IntExpr) extends Statistics
