package oscar.des.flow.lib

abstract class Time{
  def time():Double
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
abstract class DoubleExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(){value = updatedValue}
  def updatedValue():Double
  var value:Double = updatedValue
}

class MetricsStore{
  var expressions:List[Expression] = List.empty
  var accumulatingExpressions:List[Expression] = List.empty
  var nonAccumulatingExpressions:List[Expression] = List.empty
  var currentStartID = 0
  var isClosed = false
  var rootExpressions:List[(Expression,String)] = List.empty

  override def toString: String = {
    "MetricsStore{\n" +
      "   rootExpressions:" + rootExpressions.mkString(",") + "\n" +
      "   expressions:" + expressions.mkString(",") + "\n}\n"
  }

  def addMetric(e:Expression)(s:String = e.toString): Unit ={
    rootExpressions = (e,s) :: rootExpressions
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

///////////////////////////////////////////////////////////////////////////////////////////
//probe on simulation elements
case class Empty[Content<:StockContentType](s:Storage[Content]) extends BoolExpr(false){
  override def updatedValue = s.contentSize == 0
}

case class Content(s:Storage) extends DoubleExpr(false){
  override def updatedValue(): Double = s.contentSize
}

case class TotalPut(s:Storage) extends DoubleExpr(false){
  override def updatedValue(): Double = s.totalPut
}

case class TotalFetch(s:Storage) extends DoubleExpr(false){
  override def updatedValue(): Double = s.totalFetch
}

case class TotalLosByOverflow(s:Storage) extends DoubleExpr(false){
  override def updatedValue(): Double = s.totalLosByOverflow
}

case class IsRunning(p:ActivableProcess) extends BoolExpr(false){
  override def updatedValue = p.isRunning
}

case class BatchCount(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(): Double = p.batchCount
}

case class TotalWaitDuration(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(): Double = p.totalWaitDuration
}

/////////////////////////////////////////////////////////////////////////////////////////////
//logical operators
//boolean is whet it means: a boolean value at each state. there is no notion of event there; they are like fluents.
//we only consider temporal operators of the past, easy to evaluate
case class Not(f:BoolExpr) extends BoolExpr(false,f){
  override def updatedValue: Boolean = !f.value
}
case class And(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  //We cannot be lazy here because all our expression might need to be updated due to side effect.
  override def updatedValue: Boolean = if(f.value) g.value else false
}
case class Or(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  override def updatedValue: Boolean = if(f.value) true else g.value
}

//////////////////////////////////////////////////////////////////////////////////////////////
//temporal operators
/**has always ben on each query, so you have to make a qery at each time step*/
case class HasAlwaysBeen(f:BoolExpr) extends BoolExpr(true,f){
  var hasAlwaysBeen = f.value

  override def updatedValue: Boolean = if(hasAlwaysBeen) {
    hasAlwaysBeen &= f.value
    hasAlwaysBeen
  }else false
}

case class HasBeen(f:BoolExpr) extends BoolExpr(true,f){
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
class CumulatedDuration(b:BoolExpr, t:Time) extends DoubleExpr(true,b){
  var acc:Int = 0
  var wasTrue = b.value
  var previousTime = t.time

  override def updatedValue(): Double = {
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

class CurrentTime(t:Time) extends DoubleExpr(false){
  override def updatedValue(): Double = t.time
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//arithmetic operators
class Mult(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(): Double = a.value * b.value
}
class Plus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(): Double = a.value + b.value
}

//relational operators to get back to Propositions
class G(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b) {
  override def updatedValue: Boolean = a.value > b.value
}
class GE(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value >= b.value
}
class LE(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value <= b.value
}
class EQ(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value == b.value
}
class NEQ(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue: Boolean = a.value != b.value
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//temporal on integers
//triangles
class PonderateWithDuration(s:DoubleExpr,t:Time) extends DoubleExpr(true,s){
  var acc = 0
  var prevTime = t.time
  var prevValue = s.value
  override def updatedValue(): Double = {
    val now = t.time
    val nowValue = s.value
    acc += (now - prevTime) * ((nowValue + prevValue)/2)
    prevTime = now
    prevValue = nowValue
    acc
  }
}

class MaxOnHistory(s:DoubleExpr) extends DoubleExpr(true,s){
  var maxOnHistory = s.value

  override def updatedValue(): Double = {
    if (s.value > maxOnHistory){
      maxOnHistory = s.value
    }
    maxOnHistory
  }
}

class MinOnHistory(s:DoubleExpr) extends DoubleExpr(true,s){
  var minOnHistory = s.value

  override def updatedValue(): Double = {
    if (s.value < minOnHistory){
      minOnHistory = s.value
    }
    minOnHistory
  }
}

class AvgOnHistory(s:DoubleExpr,t:Time) extends DoubleExpr(true,s){
  val p = new PonderateWithDuration(s,t)

  override def updatedValue(): Double = {
    p.updatedValue
    p.value / t.time
  }
}
