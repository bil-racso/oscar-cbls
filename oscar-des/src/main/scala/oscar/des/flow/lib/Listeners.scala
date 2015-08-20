package oscar.des.flow.lib

abstract class Time{
  def time:Int
}
//This file is about thing we want to measure on the factory process

abstract class Expression(val accumulating:Boolean, val children:Expression*){
  def update()
  var id:Int = -1
}

//Variables have values at all time.
abstract class BoolExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(){value = updatedValue}
  def updatedValue:Boolean
  var value:Boolean = updatedValue
}
abstract class IntExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(){value = updatedValue}
  def updatedValue:Int
  var value:Int = updatedValue
}

//probe on simulation elements
class Empty(s:Storage) extends BoolExpr(false){
  override def updatedValue = s.contentSize == 0
  update()
}
class Productive(p:ActivableProcess) extends BoolExpr(false){
  override def updatedValue = p.isRunning
  update()
}
class Content(s:Storage) extends IntExpr(false){
  override def updatedValue: Int = s.contentSize
  update()
}

//logical properties
//boolean is whet it means: a boolean value at each state. there is no notion of event there; they are like fluents.
//we only consider temporal operators of the past, easy to evaluate
class Not(f:BoolExpr) extends BoolExpr(false,f){
  override def updatedValue: Boolean = !f.value
}
class And(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  //We cannot be lazy here because all our expression might need to be updated due to side effect.
  override def updatedValue: Boolean = if(f.value) g.value else false
}
class Or(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  override def updatedValue: Boolean = if(f.value) true else g.value
}

/**has always ben on each query, so you have to make a qery at each time step*/
class HasAlwaysBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasAlwaysBeen = f.value

  override def updatedValue: Boolean = if(hasAlwaysBeen) {
    hasAlwaysBeen &= f.value
    hasAlwaysBeen
  }else false
}

class HasBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasBeen = f.value

  override def updatedValue: Boolean = if(hasBeen) true else{
    hasBeen |= f.value
    hasBeen
  }
}

/**
 * true if exists s <= t where b AND for each u in [s,t] a holds
 * @param a
 * @param b
 */
class Since(a:BoolExpr,b:BoolExpr) extends BoolExpr(true,a,b){
  var previousValue = a.value && b.value

  override def updatedValue: Boolean = {
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

class BecomesTrue(p:BoolExpr) extends BoolExpr(true,p){
  var previousValue = p.value

  override def updatedValue: Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    !oldPreviousValue & previousValue
  }
}

class BecomesFalse(p:BoolExpr) extends BoolExpr(true,p){
  var previousValue = p.value

  override def updatedValue: Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    oldPreviousValue & !previousValue
  }
}
class Changes(p:BoolExpr) extends BoolExpr(true,p){
  var previousValue = p.value

  override def updatedValue: Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    oldPreviousValue != previousValue
  }
}

//variables always have a value.
class CumulatedDuration(b:BoolExpr, t:Time) extends IntExpr(true,b){
  var acc:Int = 0
  var wasTrue = b.value
  var previousTime = t.time

  override def updatedValue: Int = {
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

class Mult(a:IntExpr,b:IntExpr) extends IntExpr(false,a,b){
  override def updatedValue: Int = a.value * b.value
}
class Plus(a:IntExpr,b:IntExpr) extends IntExpr(false,a,b){
  override def updatedValue: Int = a.value + b.value
}
//triangles
class PonderateWithDuration(s:IntExpr,t:Time) extends IntExpr(true,s){
  var acc = 0
  var prevTime = t.time
  var prevValue = s.value
  override def updatedValue: Int = {
    val now = t.time
    var nowValue = s.value
    acc += (now - prevTime) * ((nowValue + prevValue)/2)
    prevTime = now
    prevValue = nowValue
    acc
  }
}

class MaxOnHistory(s:IntExpr) extends IntExpr(true,s){
  var maxOnHistory = s.value

  override def updatedValue: Int = {
    if (s.value > maxOnHistory){
      maxOnHistory = s.value
    }
    maxOnHistory
  }
}

class MinOnHistory(s:IntExpr) extends IntExpr(true,s){
  var minOnHistory = s.value

  override def updatedValue: Int = {
    if (s.value < minOnHistory){
      minOnHistory = s.value
    }
    minOnHistory
  }
}
class AvgOnHistory(s:IntExpr,t:Time) extends IntExpr(true,s){
  val p = new PonderateWithDuration(s,t)

  override def updatedValue: Int = {
    p.updatedValue
    p.value / t.time
  }
}

//relational operators to get back to Propositions
class G(a:IntExpr,b:IntExpr) extends BoolExpr(false,a,b) {
  override def updatedValue: Boolean = a.value > b.value
}
class GE(a:IntExpr,b:IntExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value >= b.value
}
class LE(a:IntExpr,b:IntExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value <= b.value
}
class EQ(a:IntExpr,b:IntExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value == b.value
}
class NEQ(a:IntExpr,b:IntExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value != b.value
}

//To estimate over different runs
//how to find names that are obviously statistics over different runs

class Statistics
//this only considers the latest valuee of e; at the end of the simulation run, and performs an average over several runs
class Mean(e:IntExpr) extends Statistics
class Variance(e:IntExpr) extends Statistics


class MetricsStore{
  var expressions:List[Expression] = List.empty
  var accumulatingExpressions:List[Expression] = List.empty
  var nonAccumulatingExpressions:List[Expression] = List.empty
  var currentStartID = 0
  var isClosed = false

  def addMetric(e:Expression): Unit ={
    require(!isClosed)

    def setNumbering(e:Expression, startID:Int, fatherAccumulating:Boolean):Int={
      require(e.id == -1)
      val subtreeAccumulating = fatherAccumulating || e.accumulating

      e.id = e.children.foldLeft(startID)({case (receivedID:Int,child:Expression) => setNumbering(child,receivedID,subtreeAccumulating)})
      expressions = e :: expressions
      if(subtreeAccumulating) {
        accumulatingExpressions = e :: accumulatingExpressions
      }else{
        nonAccumulatingExpressions = e :: nonAccumulatingExpressions
      }
      e.id + 1
    }

    currentStartID = setNumbering(e,currentStartID,false)
  }

  def close(){
    isClosed = true
    expressions = expressions.reverse
    accumulatingExpressions = accumulatingExpressions.reverse
    nonAccumulatingExpressions = nonAccumulatingExpressions.reverse
  }

  //to be called at each step
  def updateMetricsIfNeeded(){
    accumulatingExpressions.foreach(_.update())
  }

  //the last updateMEtrics must have been performed o nthe last state
  def finish(): Unit ={
    nonAccumulatingExpressions.foreach(_.update())
  }
}
