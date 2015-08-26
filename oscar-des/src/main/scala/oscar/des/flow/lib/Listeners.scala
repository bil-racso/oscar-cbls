package oscar.des.flow.lib

import oscar.des.engine.Model

object ExpressionStatus extends Enumeration {
  type ExpressionStatus = Value
  val Fresh, RegisteredNonAccumulating, RegisteredAccumulating = Value
}

import ExpressionStatus._

//This file is about thing we want to measure on the factory process

abstract class Expression(val accumulating:Boolean, val children:Expression*){
  def update(time:Double)
  var status:ExpressionStatus = Fresh
  def valueString:String
}

//Variables have values at all time.
abstract class BoolExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(time:Double){value = updatedValue(time)}
  def updatedValue(time:Double):Boolean
  var value:Boolean = updatedValue(0)

  override def valueString: String = "" + value
}
abstract class DoubleExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(time:Double){value = updatedValue(time)}
  def updatedValue(time:Double):Double
  var value:Double = updatedValue(0)
  override def valueString: String = "" + value
}

class MetricsStore(rootExpressions:List[(Expression,String)],verbose:Boolean){
  var expressions:List[Expression] = List.empty
  var accumulatingExpressions:List[Expression] = List.empty
  var nonAccumulatingExpressions:List[Expression] = List.empty
  var isClosed = false

  for((e,s) <- rootExpressions){
    addMetric(e)(s)
  }
  close()

  override def toString: String = {
    "MetricsStore{\n\t" + rootExpressions.map(es => es._2 + ":" + es._1.valueString).mkString("\n\t") + "\n}\n"
  }

  private def addMetric(e:Expression)(s:String = e.toString): Unit ={
    require(!isClosed)

    def registerSubExpressions(e:Expression, fatherAccumulating:Boolean){
      if(e.status == RegisteredAccumulating ||  (e.status == RegisteredNonAccumulating && !fatherAccumulating))
        return
      //so now, either it is fresh, or was registered as non accumulating, and it is now with an accumulating gather
      val subtreeAccumulating = fatherAccumulating || e.accumulating

      for(child <- e.children) {
        registerSubExpressions(child, subtreeAccumulating)
      }

      if(e.status == Fresh) expressions = e :: expressions
      if (subtreeAccumulating) {
        accumulatingExpressions = e :: accumulatingExpressions
        e.status = RegisteredAccumulating
      } else {
        e.status = RegisteredNonAccumulating
      }
    }

    registerSubExpressions(e,false)
  }

  private def close(){
    isClosed = true
    expressions = expressions.reverse
    accumulatingExpressions = accumulatingExpressions.reverse
    nonAccumulatingExpressions = expressions.filter(_.status == RegisteredNonAccumulating) //already reversed
  }

  //to be called at each step
  def updateMetricsIfNeeded(time:Double){
    if(verbose) println("updating metrics")
    accumulatingExpressions.foreach(_.update(time))
  }

  //the last updateMEtrics must have been performed o nthe last state
  def finish(time:Double){
    nonAccumulatingExpressions.foreach(_.update(time))
  }
}

///////////////////////////////////////////////////////////////////////////////////////////
//probe on simulation elements
case class Empty[Content<:StockContentType](s:Storage[Content]) extends BoolExpr(false){
  override def updatedValue(time:Double) = s.contentSize == 0
}

case class StockLevel[Content<:StockContentType](s:Storage[Content]) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.contentSize
}

case class StockCapacity[Content<:StockContentType](s:Storage[Content]) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.maxSize
}

case class RelativeStockLevel[Content<:StockContentType](s:Storage[Content]) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.contentSize.toDouble / s.maxSize.toDouble
}

case class TotalPut[Content<:StockContentType](s:Storage[Content]) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.totalPut
}

case class TotalFetch[Content<:StockContentType](s:Storage[Content]) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.totalFetch
}

case class TotalLosByOverflow[Content<:StockContentType](s:Storage[Content]) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.totalLosByOverflow
}

case class Running(p:ActivableProcess) extends BoolExpr(false){
  override def updatedValue(time:Double) = p.isRunning
}

case class CompletedBatchCount(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.completedBatchCount
}

case class StartedBatchCount(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.startedBatchCount
}

case class TotalWaitDuration(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.totalWaitDuration
}

//////////////////////////////////////////////////////////////////////////////

object AnyBatchStarted{
  def apply(p:ActivableProcess):BoolExpr = Changed(StartedBatchCount(p))
}

//////////////////////////////////////////////////////////////////////////////

case class BoolConst(b:Boolean) extends BoolExpr(false){
  override def updatedValue(time: Double): Boolean = b
}

case class DoubleConst(d:Double) extends DoubleExpr(false){
  override def updatedValue(time: Double): Double = d
}

/////////////////////////////////////////////////////////////////////////////////////////////
//logical operators
//boolean is whet it means: a boolean value at each state. there is no notion of event there; they are like fluents.
//we only consider temporal operators of the past, easy to evaluate
case class Not(f:BoolExpr) extends BoolExpr(false,f){
  override def updatedValue(time:Double): Boolean = !f.value
}
case class And(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  //We cannot be lazy here because all our expression might need to be updated due to side effect.
  override def updatedValue(time:Double): Boolean = if(f.value) g.value else false
}
case class Or(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  override def updatedValue(time:Double): Boolean = if(f.value) true else g.value
}

//////////////////////////////////////////////////////////////////////////////////////////////
//temporal operators
/**has always ben on each query, so you have to make a qery at each time step*/
case class HasAlwaysBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasAlwaysBeen = f.value

  override def updatedValue(time:Double): Boolean = if(hasAlwaysBeen) {
    hasAlwaysBeen &= f.value
    hasAlwaysBeen
  }else false
}

case class HasBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasBeen = f.value

  override def updatedValue(time:Double): Boolean = if(hasBeen) true else{
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

  override def updatedValue(time:Double): Boolean = {
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

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    !oldPreviousValue & previousValue
  }
}

class BecomesFalse(p:BoolExpr) extends BoolExpr(true,p){
  var previousValue = p.value

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    oldPreviousValue & !previousValue
  }
}

case class Changed(p:Expression) extends BoolExpr(true,p){
  def prevValueNormalized:Double = p match{
    case b:BoolExpr => if (b.value) 1.0 else 0.0
    case f:DoubleExpr => f.value
  }
  var previousValue = prevValueNormalized

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = prevValueNormalized
    oldPreviousValue != previousValue
  }
}

case class Delta(p:DoubleExpr) extends DoubleExpr(true,p){
  var previousValue = p.value

  override def updatedValue(time:Double): Double = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    previousValue - oldPreviousValue
  }
}


object CulumatedDurationNotStart {
  def apply(b: BoolExpr): DoubleExpr = {
    Minus(CumulatedDuration(b), CumulatedDuration(HasAlwaysBeen(b)))
  }
}

//variables always have a value.
case class CumulatedDuration(b:BoolExpr) extends DoubleExpr(true,b){
  var acc:Double = 0
  var wasTrue = b.value
  var previousTime:Double = 0

  override def updatedValue(time:Double): Double = {
    if(wasTrue){
      if(b.value){
        val now:Double = time
        acc += (now - previousTime)
        previousTime = now
      }else{
        wasTrue = false
      }
    }else if(b.value){
      wasTrue = true
      previousTime = time
    }
    acc
  }
}

case class CurrentTime() extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = time
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//arithmetic operators
case class Mult(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value * b.value
}

case class Plus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value + b.value
}

case class Minus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value - b.value
}

case class Div(a:DoubleExpr,b:DoubleExpr,defaultValueIfDividerIsZero:Double = Int.MaxValue) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = if(b.value == 0) defaultValueIfDividerIsZero else (a.value / b.value)
}

//relational operators to get back to Propositions
case class G(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b) {
  override def updatedValue(time:Double): Boolean = a.value > b.value
}

case class GE(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value >= b.value
}

case class LE(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value <= b.value
}

case class EQ(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value == b.value
}

case class NEQ(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value != b.value
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//temporal on integers
//triangles
case class PonderateWithDuration(s:DoubleExpr) extends DoubleExpr(true,s){
  var acc:Double = 0
  var prevTime:Double = 0
  var prevValue:Double = s.value
  override def updatedValue(time:Double): Double = {
    val now = time
    val nowValue = s.value
    acc += (now - prevTime) * ((nowValue + prevValue)/2)
    prevTime = now
    prevValue = nowValue
    acc
  }
}

case class MaxOnHistory(s:DoubleExpr, when:BoolExpr = null) extends DoubleExpr(true,(if(when == null) List(s) else List(s,when)):_*){
  var maxOnHistory = s.value

  override def updatedValue(time:Double): Double = {
    if (s.value > maxOnHistory && (when == null || when.value)){
      maxOnHistory = s.value
    }
    maxOnHistory
  }

  def when(b:BoolExpr): Unit ={
    if(this.when == null){
      MaxOnHistory(s, when)
    }else{
      MaxOnHistory(s, And(when,this.when))
    }
  }
}

case class MinOnHistory(s:DoubleExpr) extends DoubleExpr(true,s){
  var minOnHistory = s.value

  override def updatedValue(time:Double): Double = {
    if (s.value < minOnHistory){
      minOnHistory = s.value
    }
    minOnHistory
  }
}

object AvgOnHistory{
  def apply (s:DoubleExpr):DoubleExpr = {
    Div(PonderateWithDuration(s),CurrentTime())
  }
}
