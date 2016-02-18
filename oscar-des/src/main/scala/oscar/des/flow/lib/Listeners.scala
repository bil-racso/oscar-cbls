package oscar.des.flow.lib

object ExpressionStatus extends Enumeration {
  type ExpressionStatus = Value
  val Fresh, RegisteredNonAccumulating, RegisteredAccumulating = Value
}

import ExpressionStatus._

//This file is about thing we want to measure on the factory process

sealed abstract class Expression(val accumulating:Boolean, val children:Expression*){
  def update(time:Double)
  var status:ExpressionStatus = Fresh
  def valueString:String
  def reset()
}

//Variables have values at all time.
abstract class BoolExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(time:Double){value = updatedValue(time)}
  def updatedValue(time:Double):Boolean
  var value:Boolean = updatedValue(0)

  override def valueString: String = "" + value
  def reset() = {
    if(accumulating) resetAccumulators()
    value = updatedValue(0)
  }

  def resetAccumulators() {
    throw new Error("accumulating expression must have reset accumulator method overriden")
  }
  def cloneReset(boolMap:Map[BoolExpr,BoolExpr],doubleMap:Map[BoolExpr,BoolExpr], storeMap:Map[Storage,Storage],processMap:Map[ActivableProcess,ActivableProcess]):BoolExpr
}

abstract class DoubleExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(time:Double){value = updatedValue(time)}
  def updatedValue(time:Double):Double
  var value:Double = updatedValue(0)
  override def valueString: String = "" + value

  def reset() = {
    if(accumulating) resetAccumulators()
    value = updatedValue(0)
  }

  def resetAccumulators(): Unit = {
    throw new Error("accumulating expressions must implement resetAccumulators")
  }
  def cloneReset(boolMap:Map[BoolExpr,BoolExpr],doubleMap:Map[BoolExpr,BoolExpr], storeMap:Map[Storage,Storage],processMap:Map[ActivableProcess,ActivableProcess]):DoubleExpr
}


//class TimedDoubleExpr(child:DoubleExpr) extends Expression(true,child){
//  override def update(time: Double): Unit = ???
//
//  override def valueString: String = ???
//
//  override def reset(): Unit = ???
//}


class MetricsStore(rootExpressions:List[(String, Expression)],verbosity:String=>Unit){
  var expressions:List[Expression] = List.empty
  var accumulatingExpressions:List[Expression] = List.empty
  var nonAccumulatingExpressions:List[Expression] = List.empty
  var isClosed = false

  for((e,s) <- rootExpressions){
    addMetric(s)(e)
  }
  close()

  override def toString: String = {
    "MetricsStore{\n\t" + rootExpressions.map(es => es._1 + ":" + es._2.valueString).mkString("\n\t") + "\n}\n"
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
    if(verbosity!=null) verbosity("updating metrics")
    var currentExpressionList = accumulatingExpressions
    while(currentExpressionList.nonEmpty){
      currentExpressionList.head.update(time)
      currentExpressionList = currentExpressionList.tail
    }
  }

  //the last updateMEtrics must have been performed o nthe last state
  def finish(time:Double){
    nonAccumulatingExpressions.foreach(_.update(time))
  }

  def reset(){
    accumulatingExpressions.map(_.reset())
  }
}

///////////////////////////////////////////////////////////////////////////////////////////
//probe on simulation element

/**
 * true if the storage is empty, false otherwise
 * @param s a storage
 */
case class Empty(s:Storage) extends BoolExpr(false){
  override def updatedValue(time:Double) = s.contentSize == 0

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
  Empty(storeMap(s))
}

/**
 * the level of the storage s in number of units
 * @param s a storage
 */
case class StockLevel(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.contentSize

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    StockLevel(storeMap(s))
}

/**
 * the maximal number of item that can be put in the storage s
 * @param s a storage
 */
case class StockCapacity(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.maxCapacity

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
  StockCapacity(storeMap(s))
}

/**
 * the relative level of the storage s (number of item / maximal capacity)
 * @param s a storage
 */
case class RelativeStockLevel(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.contentSize.toDouble / s.maxCapacity.toDouble

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
  RelativeStockLevel(storeMap(s))
}

/**
 * the number of items that have been put in the storage since the beginning of the simulation, not counting the initial ones
 * @param s a storage
 */
case class TotalPut(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.totalPut

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    TotalPut(storeMap(s))
}

/**
 * the number of items that have been taken out of the storage since the beginning of the trace
 * @param s a storage
 */
case class TotalFetch(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.totalFetch

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
  TotalFetch(storeMap(s))
}

/**
 * the number of items that have been lost by the storage through overflow. obviously zero if the storage does not overflow.
 * @param s a storage
 */
case class TotalLosByOverflow(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.totalLosByOverflow

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
  TotalLosByOverflow(storeMap(s))
}

/**
 * true if the process is running, false otherwise. a process might not be running if it is blocked by lack of input material or output storage blocking the outputting of items at the output of the process.
 * notice that [[ConveyorBeltProcess]] do not block by lack of input items
 * for a process with multiple lines, this returns true iff if at least one line is running
 * @param p a process
 */
case class Running(p:ActivableProcess) extends BoolExpr(false){
  override def updatedValue(time:Double) = p.isRunning

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    Running(processMap(p))
}

/**
 * the total number of batches performed by the process since the beginning of the trace
 * a splitting process sums up the batches delivered to each of its outputs
 * for a process with multiple lines, it sums up the completed batches of each line.
 * @param p a process
 */
case class CompletedBatchCount(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.completedBatchCount

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    CompletedBatchCount(processMap(p))
}

//TODO: distinguish batches from splitting process?

/**
 * the number of batches started by the process since the beginning of the trace
 *  * for a process with multiple lines, it sums up the started batches of each line.
 * @param p a process
 */
case class StartedBatchCount(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.startedBatchCount

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
  StartedBatchCount(processMap(p))
}

/**
 * the total time where the process was not running since the beginning of the trace.
 * for a process with multiple lines, it sums up the waiting time of each line.
 * @param p a process
 */
case class TotalWaitDuration(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.totalWaitDuration

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
  TotalWaitDuration(processMap(p))
}


//////////////////////////////////////////////////////////////////////////////

/**
 * a boolean constant
 * @param b the value of the constant
 */
case class BoolConst(b:Boolean) extends BoolExpr(false){
  override def updatedValue(time: Double): Boolean = b

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr = this
}

/**
 * a Double constant
 * @param d the value of the constant
 */
case class DoubleConst(d:Double) extends DoubleExpr(false){
  override def updatedValue(time: Double): Double = d

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr = this
}

/////////////////////////////////////////////////////////////////////////////////////////////
//logical operators
//boolean is whet it means: a boolean value at each state. there is no notion of event there; they are like fluents.

/**
 * the negation of expression f
 * @param f a boolean expression
 */
case class Not(f:BoolExpr) extends BoolExpr(false,f){
  override def updatedValue(time:Double): Boolean = !f.value

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr = Not(boolMap(f))
}

/**
 * the conjunction of two boolean expressions
 * @param f a boolean expression
 * @param g a boolean expression
 */
case class And(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  //We cannot be lazy here because all our expression might need to be updated due to side effect.
  override def updatedValue(time:Double): Boolean = if(f.value) g.value else false

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
  And(boolMap(f),boolMap(g))
}

/**
 * the disjunction between two boolean expressions
 * @param f a boolean expression
 * @param g a boolean expression
 */
case class Or(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  override def updatedValue(time:Double): Boolean = if(f.value) true else g.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    Or(boolMap(f),boolMap(g))
}

case class ITE(i:BoolExpr, t:BoolExpr, e:BoolExpr ) extends BoolExpr(false,i,t,e){
  override def updatedValue(time: Double): Boolean = if(i.value) t.value else e.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    ITE(boolMap(i),boolMap(t),boolMap(e))
}
//////////////////////////////////////////////////////////////////////////////////////////////
//temporal operators
//we only consider temporal operators of the past, easy to evaluate


/**
 * true if f has always been true since the start of the trace
 * @param f a boolean expression
 */
case class HasAlwaysBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasAlwaysBeen = f.value

  override def updatedValue(time:Double): Boolean = if(hasAlwaysBeen) {
    hasAlwaysBeen &= f.value
    hasAlwaysBeen
  }else false

  override def resetAccumulators(): Unit = {
    hasAlwaysBeen = f.value
  }

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
  HasAlwaysBeen(boolMap(f))
}

/**
 * true if there is a post position (now is included) such that f is true at this position
 * @param f a boolean expression
 */
case class HasBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasBeen = f.value

  override def updatedValue(time:Double): Boolean = if(hasBeen) true else{
    hasBeen |= f.value
    hasBeen
  }

  override def resetAccumulators(): Unit = {
    hasBeen = f.value
  }
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    HasBeen(boolMap(f))
}

/**
 * true if there is a past position in time (now is included)
 * such that b is true and for each position between this position and now (both being included) a holds
 * @param a a boolean expression
 * @param b a boolean expression
 */
case class Since(a:BoolExpr,b:BoolExpr) extends BoolExpr(true,a,b){
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

  override def resetAccumulators(): Unit = {
    previousValue = a.value && b.value
  }

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
  Since(boolMap(a),boolMap(b))
}

/**
 * true iff p is true now, and was false at the previous step in the trace
 * BEWARE that this is a dangerous epxression, since time is event-based,
 * so that an additional artifact in the model might introduce additional intermediary steps)
 * @param p
 */
case class BecomesTrue(p:BoolExpr) extends BoolExpr(true,p){
  var previousValue = p.value

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    !oldPreviousValue & previousValue
  }

  override def resetAccumulators(): Unit = {
    previousValue = p.value
  }

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[BoolExpr, BoolExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
  BecomesTrue(boolMap(p))
}

/**
 * true whenever the value of p changes, that is, whenever it is different from its value at the previous itration step
 * BEWARE that this is a dangerous expression, since time is event-based,
 * so that an additional artifact in the model might introduce additional intermediary steps)
 * @param p an expression; it might be a boolean or a double expression
 */
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
  override def resetAccumulators(): Unit = {
    previousValue = prevValueNormalized
  }
}

/**
 * the value of d now minus the value of p at the previous position in the trace.
 * BEWARE that this is a dangerous expression, since time is event-based,
 * so that an additional artifact in the model might introduce additional intermediary steps)
 * @param p a double expression
 */
case class Delta(p:DoubleExpr) extends DoubleExpr(true,p){
  var previousValue = p.value

  override def updatedValue(time:Double): Double = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    previousValue - oldPreviousValue
  }
  override def resetAccumulators(): Unit = {
    previousValue = p.value
  }
}

/**
 * the sum of the time lapse between consecutive positions in the trace where b is true at both positions
 * @param b a boolean expression
 */
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
  override def resetAccumulators(): Unit = {
    acc = 0
    wasTrue = b.value
    previousTime = 0
  }
}

/**the duration for which b has been true since it was last false*/
case class Duration(b:BoolExpr) extends DoubleExpr(true,b){
  var bWasTrueOnLastCall:Boolean = b.value
  var timeWhenBStartedToBeTrue:Double = 0
  override def updatedValue(time: Double): Double = {
    if(bWasTrueOnLastCall) {
      if (b.value) {
        time - timeWhenBStartedToBeTrue
      } else {
        bWasTrueOnLastCall = false
        0
      }
    }else{
      if (b.value) {
        bWasTrueOnLastCall = true
      }
      0
    }
  }
  override def resetAccumulators(): Unit = {
    bWasTrueOnLastCall = b.value
    timeWhenBStartedToBeTrue = 0
  }
}

/**
 * the duretion between the start of history (time zero) and the curret position in time
 */
case class CurrentTime() extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = time
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//arithmetic operators

/**
 * a * b
 * @param a a double expression
 * @param b a double expression
 */
case class Mult(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value * b.value
}

/**
 * a + b
 * @param a a double expression
 * @param b a double expression
 */
case class Plus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value + b.value
}

/**
 * a - b
 * @param a a double expression
 * @param b a double expression
 */
case class Minus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value - b.value
}

/**
 * if (b == 0) defaultValueIfDividerIsZero else a / b
 * if defaultValueIfDividerIsZero is Nan, throws an exception in case of divide by zero
 * @param a a double expression
 * @param b a double expression
 * @param defaultValueIfDividerIsZero the default value for this if b is zero
 */
case class Div(a:DoubleExpr,b:DoubleExpr,defaultValueIfDividerIsZero:Double = Double.NaN) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = if(b.value == 0 && !defaultValueIfDividerIsZero.isNaN) defaultValueIfDividerIsZero else (a.value / b.value)
}

/**
 * a > b
 * @param a a double expression
 * @param b a double expression
 */
case class G(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b) {
  override def updatedValue(time:Double): Boolean = a.value > b.value
}

/**
 * a >= b
 * @param a a double expression
 * @param b a double expression
 */
case class GE(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value >= b.value
}

/**
 * a == b
 * @param a a double expression
 * @param b a double expression
 */
case class EQ(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value == b.value
}

case class DoubleITE(i:BoolExpr, t:DoubleExpr, e:DoubleExpr) extends DoubleExpr(false,i,t,e){
  override def updatedValue(time: Double): Double = if(i.value) t.value else e.value
}

case class BoolSubExpression(name:String,expr:BoolExpr) extends BoolExpr(false,expr){
  override def updatedValue(time: Double): Boolean = expr.value
}

case class DoubleSubExpression(name:String,expr:DoubleExpr) extends DoubleExpr(false,expr){
  override def updatedValue(time: Double): Double = expr.value
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//temporal on integers
//triangles

/**
 * computes the integral of s over time through the trapezoidal rule
 * taking the events as discretizations base
 * https://en.wikipedia.org/wiki/Numerical_integration
 * @param s a double expression
 */
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
  override def resetAccumulators(): Unit = {
    acc = 0
    prevTime = 0
    prevValue = s.value
  }
}

/**
 * the max value of s over all position of time since the start where "when" is true
 * @param s the measured expression
 * @param when a boolean expression telling the relevant point of time to consider
 */
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
  override def resetAccumulators(): Unit = {
    maxOnHistory = s.value
  }
}


