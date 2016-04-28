package oscar.des.flow.lib

sealed abstract class ExpressionStatus(val i:Int) extends Ordered[ExpressionStatus]
case object Fresh extends ExpressionStatus(0) {
  override def compare(that: ExpressionStatus): Int = {
    if(that.i == 0) throw new Error("canot compare fresh Expressions")
    1
  }
}
case class RegisteredNonAccumulating(id:Int) extends ExpressionStatus(1){
  override def compare(that: ExpressionStatus): Int = {
    that match{
      case Fresh => -1
      case r:RegisteredNonAccumulating => id.compare(r.id)
      case _:RegisteredAccumulating => 1
    }
  }
}
case class RegisteredAccumulating(id:Int) extends ExpressionStatus(2) {
  override def compare(that: ExpressionStatus): Int = {
    that match{
      case Fresh => -1
      case _:RegisteredNonAccumulating => -1
      case r:RegisteredAccumulating => id.compare(r.id)
    }
  }
}

import oscar.des.flow.core.AttributeCondition

import scala.collection.immutable.SortedMap

//This file is about thing we want to measure on the factory process

class OrderingOnExpression[T<:Expression]() extends Ordering[T]{
  override def compare(x: T, y: T): Int = x.compare(y)
}

sealed abstract class Expression(val accumulating:Boolean, val children:Expression*) extends Ordered[Expression]{
  def update(time:Double)
  var status:ExpressionStatus = Fresh
  def valueString:String

  override def compare(that: Expression): Int = {
    this.status.compare(that.status)
  }
}

abstract class BoolExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*){
  override def update(time:Double){value = updatedValue(time)}
  def updatedValue(time:Double):Boolean
  var value:Boolean = updatedValue(0)

  override def valueString: String = "" + value

  def cloneReset(boolMap:Map[BoolExpr,BoolExpr],doubleMap:Map[DoubleExpr,DoubleExpr], storeMap:Map[Storage,Storage],processMap:Map[ActivableProcess,ActivableProcess]):BoolExpr

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  def cloneExpr:(BoolExpr,Boolean)
}

abstract class DoubleExpr(accumulating:Boolean, children:Expression*) extends Expression(accumulating,children:_*) {
  override def update(time:Double){value = updatedValue(time)}
  def updatedValue(time:Double):Double
  var value:Double = updatedValue(0)
  override def valueString: String = "" + value

  def cloneReset(boolMap:Map[BoolExpr,BoolExpr],doubleMap:Map[DoubleExpr,DoubleExpr], storeMap:Map[Storage,Storage],processMap:Map[ActivableProcess,ActivableProcess]):DoubleExpr

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  def cloneExpr:(DoubleExpr,Boolean)
}

case class DoubleHistoryExpr(child:DoubleExpr) extends Expression(true,child){
  private var history:List[(Double,Double)] = List.empty
  override def update(time: Double){
    val newValue = child.value
    history match{
      case (t1,v1) :: (t2,v2) :: tail if newValue == v2 && v1 == newValue =>
        history = (time,newValue) :: (t2,newValue) :: tail
      case _ => history = (time,newValue) :: history
    }
  }

  override def valueString: String = "(size:" + history.size + "):" + history.reverse

  def cloneReset(boolMap:Map[BoolExpr,BoolExpr],
                 doubleMap:Map[DoubleExpr,DoubleExpr],
                 storeMap:Map[Storage,Storage],
                 processMap:Map[ActivableProcess,ActivableProcess]):DoubleHistoryExpr = new DoubleHistoryExpr(doubleMap(child))
}

case class BoolHistoryExpr(child:BoolExpr) extends Expression(true,child){
  private var history:List[(Double,Boolean)] = List.empty
  override def update(time: Double){
    val newValue = child.value
    history match{
      case (t1,v1) :: (t2,v2) :: tail if newValue == v2 && v1 == newValue => history = (time,newValue) :: (t2,newValue) :: tail
      case _ => history = (time,child.value) :: history
    }
  }

  override def valueString: String = "" + history.reverse

  def cloneReset(boolMap:Map[BoolExpr,BoolExpr],
                 doubleMap:Map[DoubleExpr,DoubleExpr],
                 storeMap:Map[Storage,Storage],
                 processMap:Map[ActivableProcess,ActivableProcess]):BoolHistoryExpr = new BoolHistoryExpr(boolMap(child))
}

class MetricsStore(val rootExpressions:List[(String, Expression)],verbosity:String=>Unit,
                   var accumulatingExpressions:Array[Expression] = null,
                   var nonAccumulatingExpressions:Array[Expression] = null,
                   var expressions:List[Expression] = List.empty){

  var isClosed = false
  if(accumulatingExpressions == null) {
    require(nonAccumulatingExpressions == null)
    require(expressions.isEmpty)
    for ((e, s) <- rootExpressions) {
      addMetric(s)(e)
    }
    close()
  }else{
    isClosed = true
  }

  override def toString: String = {
    "MetricsStore{\n\t" + rootExpressions.map(es => es._1 + ":" + es._2.valueString).mkString("\n\t") + "\n}\n"
  }

  private def addMetric(e:Expression)(s:String = e.toString): Unit ={
    require(!isClosed)

    def registerSubExpressions(e:Expression, fatherAccumulating:Boolean){
      if(e.status.isInstanceOf[RegisteredAccumulating] ||  (e.status.isInstanceOf[RegisteredNonAccumulating] && !fatherAccumulating))
        return
      //so now, either it is fresh, or was registered as non accumulating, and it is now with an accumulating gather
      val subtreeAccumulating = fatherAccumulating || e.accumulating

      for(child <- e.children) {
        registerSubExpressions(child, subtreeAccumulating)
      }

      if(e.status == Fresh) expressions = e :: expressions
      if (subtreeAccumulating) {
        e.status = RegisteredAccumulating(0)
      } else {
        e.status = RegisteredNonAccumulating(0)
      }
    }

    registerSubExpressions(e,false)
  }

  private def close(){
    isClosed = true
    expressions = expressions.reverse

    //TODO: can be much faster, in 1 pass instead of 5
    val accumulatingExpressionList = expressions.filter(_.status.isInstanceOf[RegisteredAccumulating])
    var currentPos = accumulatingExpressionList
    accumulatingExpressions = Array.fill(accumulatingExpressionList.size)(null)
    var i = 0
    while(currentPos.nonEmpty){
      currentPos.head.status = RegisteredAccumulating(i)
      accumulatingExpressions(i) = currentPos.head
      i+=1
      currentPos = currentPos.tail
    }

    val nonAccumulatingExpressionList = expressions.filter(_.status.isInstanceOf[RegisteredNonAccumulating])
    currentPos = nonAccumulatingExpressionList
    nonAccumulatingExpressions = Array.fill(nonAccumulatingExpressionList.size)(null)
    i = 0
    while(currentPos.nonEmpty){
      currentPos.head.status = RegisteredNonAccumulating(i)
      nonAccumulatingExpressions(i) = currentPos.head
      i+=1
      currentPos = currentPos.tail
    }
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

  //the last updateMetrics must have been performed on the last state
  def finish(time:Double){
    nonAccumulatingExpressions.foreach(_.update(time))
  }

  def cloneReset(storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]):(MetricsStore,Map[DoubleExpr,DoubleExpr],Map[BoolExpr,BoolExpr]) = {

    var translatedBoolExpr:Map[BoolExpr,BoolExpr] = SortedMap.empty[BoolExpr,BoolExpr](new OrderingOnExpression[BoolExpr])
    var translatedDoubleExpr:Map[DoubleExpr,DoubleExpr] = SortedMap.empty[DoubleExpr,DoubleExpr](new OrderingOnExpression[DoubleExpr])
    var translatedDH:Map[DoubleHistoryExpr,DoubleHistoryExpr] = SortedMap.empty[DoubleHistoryExpr,DoubleHistoryExpr](new OrderingOnExpression[DoubleHistoryExpr])
    var translatedBH:Map[BoolHistoryExpr,BoolHistoryExpr]= SortedMap.empty[BoolHistoryExpr,BoolHistoryExpr](new OrderingOnExpression[BoolHistoryExpr])

    def getTranslated(e:Expression):Expression = {
      e match{
        case b:BoolExpr => translatedBoolExpr(b)
        case d:DoubleExpr => translatedDoubleExpr(d)
        case dh:DoubleHistoryExpr => translatedDH(dh)
        case bh:BoolHistoryExpr => translatedBH(bh)
      }
    }

    def setTranslated(e:Expression,translation:Expression) = {
      (e,translation) match{
        case (a:BoolExpr,b:BoolExpr) => translatedBoolExpr = translatedBoolExpr + ((a,b))
        case (a:DoubleExpr,b:DoubleExpr) => translatedDoubleExpr = translatedDoubleExpr + ((a,b))
        case (a:DoubleHistoryExpr,b:DoubleHistoryExpr) => translatedDH = translatedDH + ((a,b))
        case (a:BoolHistoryExpr,b:BoolHistoryExpr) => translatedBH = translatedBH + ((a,b))
        case _ => throw new Error("non-marching translation")
      }
    }

    def performTranslation(e:Expression):Expression = {
      e match{
        case b:BoolExpr => b.cloneReset(translatedBoolExpr,translatedDoubleExpr,storeMap,processMap)
        case d:DoubleExpr => d.cloneReset(translatedBoolExpr,translatedDoubleExpr,storeMap,processMap)
        case dh:DoubleHistoryExpr => dh.cloneReset(translatedBoolExpr,translatedDoubleExpr,storeMap,processMap)
        case bh:BoolHistoryExpr => bh.cloneReset(translatedBoolExpr,translatedDoubleExpr,storeMap,processMap)
      }
    }

    val translatedExpression = expressions.map(e => {
      val translated = performTranslation(e)
      setTranslated(e,translated)
      translated
    })

    var currentID = 0
    val translatedAccumulatingExpressions = accumulatingExpressions.map(e =>{
      val translated = getTranslated(e)
      translated.status = RegisteredAccumulating(currentID)
      currentID += 1
      translated
    })
    currentID = 0
    val translatedNonAccumulatingExpressions = nonAccumulatingExpressions.map(e => {
      val translated = getTranslated(e)
      translated.status = RegisteredNonAccumulating(currentID)
      currentID += 1
      translated
    })

    (new MetricsStore(rootExpressions.map({case (name,e) => (name,getTranslated(e))}),
      verbosity,
      translatedAccumulatingExpressions,
      translatedNonAccumulatingExpressions,
      translatedExpression),translatedDoubleExpr,translatedBoolExpr)
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    Empty(storeMap(s))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = s.cloneStorage match{case (t,true) => (Empty(t),true) case _ => (this,false)}
}

/**
 * the level of the storage s in number of units
 * @param s a storage
 */
case class StockLevel(s:Storage, a:Option[AttributeCondition]) extends DoubleExpr(false){
  val specificCondition:Boolean = a match{case None => false case Some(_) => true}
  val measurer = if(specificCondition) s.measureCondition(a.head) else null

  override def updatedValue(time:Double): Double = if(specificCondition) measurer.content else s.contentSize

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr],
                          doubleMap: Map[DoubleExpr, DoubleExpr],
                          storeMap: Map[Storage, Storage],
                          processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    StockLevel(storeMap(s),a)

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneStorage match{case (t,true) => (StockLevel(t,a),true) case _ => (this,false)}
}

/**
 * the maximal number of item that can be put in the storage s
 * @param s a storage
 */
case class StockCapacity(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.maxCapacity

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    StockCapacity(storeMap(s))

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneStorage match{case (t,true) => (StockCapacity(t),true) case _ => (this,false)}
}

/**
 * the relative level of the storage s (number of item / maximal capacity)
 * @param s a storage
 */
case class RelativeStockLevel(s:Storage) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = s.contentSize.toDouble / s.maxCapacity.toDouble

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    RelativeStockLevel(storeMap(s))

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneStorage match{case (t,true) => (RelativeStockLevel(t),true) case _ => (this,false)}
}

/**
 * the number of items that have been put in the storage since the beginning of the simulation, not counting the initial ones
 * @param s a storage
 */
case class TotalPut(s:Storage, a:Option[AttributeCondition]) extends DoubleExpr(false){
  val specificCondition:Boolean = a match{case None => false case Some(_) => true}
  val measurer = if(specificCondition) s.measureCondition(a.head) else null

  override def updatedValue(time:Double): Double = if(specificCondition) measurer.puts else s.totalPut

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    TotalPut(storeMap(s),a)

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneStorage match{case (t,true) => (TotalPut(t,a),true) case _ => (this,false)}
}

/**
 * the number of items that have been taken out of the storage since the beginning of the trace
 * @param s a storage
 */
case class TotalFetch(s:Storage, a:Option[AttributeCondition]) extends DoubleExpr(false){
  val specificCondition:Boolean = a match{case None => false case Some(_) => true}
  val measurer = if(specificCondition) s.measureCondition(a.head) else null

  override def updatedValue(time:Double): Double = if(specificCondition) measurer.gets else s.totalFetch


  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    TotalFetch(storeMap(s),a)

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneStorage match{case (t,true) => (TotalFetch(t,a),true) case _ => (this,false)}
}

/**
 * the number of items that have been lost by the storage through overflow. obviously zero if the storage does not overflow.
 * @param s a storage
 */
case class TotalLosByOverflow(s:Storage, a:Option[AttributeCondition]) extends DoubleExpr(false){
  val specificCondition:Boolean = a match{case None => false case Some(_) => true}
  val measurer = if(specificCondition) s.measureCondition(a.head) else null

  override def updatedValue(time:Double): Double = if(specificCondition) measurer.overflow else s.totalLosByOverflow

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    TotalLosByOverflow(storeMap(s),a)

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneStorage match{case (t,true) => (TotalLosByOverflow(t,a),true) case _ => (this,false)}
}

/**
 * true if the process is running, false otherwise. a process might not be running if it is blocked by lack of input material or output storage blocking the outputting of items at the output of the process.
 * notice that [[ConveyorBeltProcess]] do not block by lack of input items
 * for a process with multiple lines, this returns true iff if at least one line is running
 * @param p a process
 */
case class Running(p:ActivableProcess) extends BoolExpr(false){
  override def updatedValue(time:Double) = p.isRunning

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    Running(processMap(p))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = p.cloneProcess match{
    case (q,true) => (Running(q),true)
    case _ => (this,false)
  }
}

/**
 * the total number of batches performed by the process since the beginning of the trace
 * a splitting process sums up the batches delivered to each of its outputs
 * for a process with multiple lines, it sums up the completed batches of each line.
 * @param p a process
 * @param outputPortNumber the port number to consider. If -1 it will consider the sum over all ports.
 */
case class CompletedBatchCount(p:ActivableProcess, outputPortNumber:Int) extends DoubleExpr(false){
  require(outputPortNumber == -1 || p.isInstanceOf[SplittingBatchProcess] || p.isInstanceOf[SplittingSingleBatchProcess])

  override def updatedValue(time:Double): Double = p.completedBatchCount(outputPortNumber)

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    CompletedBatchCount(processMap(p),outputPortNumber)

  override def cloneExpr : (DoubleExpr, Boolean) = {
    p.cloneProcess match{
      case (q,true) => (CompletedBatchCount(q,outputPortNumber),true)
      case (q,false) => (this,false)
    }
  }
}

//TODO: distinguish batches from splitting process?

/**
 * the number of batches started by the process since the beginning of the trace
 *  * for a process with multiple lines, it sums up the started batches of each line.
 * @param p a process
 */
case class StartedBatchCount(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.startedBatchCount

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    StartedBatchCount(processMap(p))

  override def cloneExpr : (DoubleExpr, Boolean) = {
    p.cloneProcess match{
      case (q,true) => (StartedBatchCount(q),true)
      case (q,false) => (this,false)
    }
  }
}

/**
 * the total time where the process was not running since the beginning of the trace.
 * for a process with multiple lines, it sums up the waiting time of each line.
 * @param p a process
 */
case class TotalWaitDuration(p:ActivableProcess) extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = p.totalWaitDuration

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    TotalWaitDuration(processMap(p))

  override def cloneExpr : (DoubleExpr, Boolean) = {
    p.cloneProcess match{
      case (q,true) => (TotalWaitDuration(q),true)
      case (q,false) => (this,false)
    }
  }
}


//////////////////////////////////////////////////////////////////////////////

/**
 * a boolean constant
 * @param b the value of the constant
 */
case class BoolConst(b:Boolean) extends BoolExpr(false){
  override def updatedValue(time: Double): Boolean = b

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr = this

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = (this,false)
}

/**
 * a Double constant
 * @param d the value of the constant
 */
case class DoubleConst(d:Double) extends DoubleExpr(false){
  override def updatedValue(time: Double): Double = d

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr],
                          doubleMap: Map[DoubleExpr, DoubleExpr],
                          storeMap: Map[Storage, Storage],
                          processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr = this

  override def cloneExpr : (DoubleExpr, Boolean) = (this,false)
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr],
                          doubleMap: Map[DoubleExpr, DoubleExpr],
                          storeMap: Map[Storage, Storage],
                          processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr = Not(boolMap(f))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = f.cloneExpr match{case (g,true) => (Not(g),true) case _ => (this,false)}
}

/**
 * the conjunction of two boolean expressions
 * @param f a boolean expression
 * @param g a boolean expression
 */
case class And(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  //We cannot be lazy here because all our expression might need to be updated due to side effect.
  override def updatedValue(time:Double): Boolean = if(f.value) g.value else false

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    And(boolMap(f),boolMap(g))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = (f.cloneExpr,g.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (And(a._1,b._1),true)
  }
}

/**
 * the disjunction between two boolean expressions
 * @param f a boolean expression
 * @param g a boolean expression
 */
case class Or(f:BoolExpr, g:BoolExpr) extends BoolExpr(false,f,g){
  override def updatedValue(time:Double): Boolean = if(f.value) true else g.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    Or(boolMap(f),boolMap(g))

  override def cloneExpr : (BoolExpr, Boolean) = (f.cloneExpr,g.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (Or(a._1,b._1),true)
  }
}

case class ITE(i:BoolExpr, t:BoolExpr, e:BoolExpr ) extends BoolExpr(false,i,t,e){
  override def updatedValue(time: Double): Boolean = if(i.value) t.value else e.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    ITE(boolMap(i),boolMap(t),boolMap(e))

  override def cloneExpr : (BoolExpr, Boolean) = (i.cloneExpr,t.cloneExpr,e.cloneExpr) match{
    case ((ii,false),(tt,false),(ee,false)) => (this,false)
    case (ii,tt,ee) => (ITE(ii._1,tt._1,ee._1),true)
  }

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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    HasAlwaysBeen(boolMap(f))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = f.cloneExpr match{case (g,true) => (HasAlwaysBeen(g),true) case _ => (this,false)}
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    HasBeen(boolMap(f))

  override def cloneExpr : (BoolExpr, Boolean) = f.cloneExpr match{case (g,true) => (HasBeen(g),true) case _ => (this,false)}
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    Since(boolMap(a),boolMap(b))

  override def cloneExpr : (BoolExpr, Boolean) = (a.cloneExpr,a.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (Since(a._1,b._1),true)
  }
}

/**
 * true iff p is true now, and was false at the previous step in the trace
 * BEWARE that this is a dangerous expression, since time is event-based,
 * so that an additional artifact in the model might introduce additional intermediary steps)
 * @param p a boolean expression
 */
case class BecomesTrue(p:BoolExpr) extends BoolExpr(true,p){
  var previousValue = p.value

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = p.value
    !oldPreviousValue & previousValue
  }

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    BecomesTrue(boolMap(p))

  override def cloneExpr : (BoolExpr, Boolean) = p.cloneExpr match{case (g,true) => (BecomesTrue(g),true) case _ => (this,false)}
}

/**
 * true whenever the value of p changes, that is, whenever it is different from its value at the previous itration step
 * BEWARE that this is a dangerous expression, since time is event-based,
 * so that an additional artifact in the model might introduce additional intermediary steps)
 * @param b an expression; it might be a boolean or a double expression
 */
case class BoolChanged(b:BoolExpr) extends BoolExpr(true,b){
  def prevValueNormalized:Double =if (b.value) 1.0 else 0.0
  var previousValue = prevValueNormalized

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = prevValueNormalized
    oldPreviousValue != previousValue
  }

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    BoolChanged(boolMap(b))

  override def cloneExpr : (BoolExpr, Boolean) = b.cloneExpr match{case (g,true) => (BoolChanged(g),true) case _ => (this,false)}
}

/**
 * true whenever the value of p changes, that is, whenever it is different from its value at the previous itration step
 * BEWARE that this is a dangerous expression, since time is event-based,
 * so that an additional artifact in the model might introduce additional intermediary steps)
 * @param p an expression; it might be a boolean or a double expression
 */
case class DoubleChanged(p:DoubleExpr) extends BoolExpr(true,p){
  def prevValueNormalized:Double = p.value
  var previousValue = prevValueNormalized

  override def updatedValue(time:Double): Boolean = {
    val oldPreviousValue = previousValue
    previousValue = prevValueNormalized
    oldPreviousValue != previousValue
  }

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    DoubleChanged(doubleMap(p))

  override def cloneExpr : (BoolExpr, Boolean) = p.cloneExpr match{case (g,true) => (DoubleChanged(g),true) case _ => (this,false)}
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    Delta(doubleMap(p))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (DoubleExpr, Boolean) = p.cloneExpr match{
    case (q,true) => (Delta(q),true)
    case (_,false) => (this,false)
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    CumulatedDuration(boolMap(b))

  override def cloneExpr : (DoubleExpr, Boolean) = b.cloneExpr match{
    case (q,true) => (CumulatedDuration(q),true)
    case (_,false) => (this,false)
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    Duration(boolMap(b))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (DoubleExpr, Boolean) = {
    b.cloneExpr match{
      case (b,true) => (Duration(b),true)
      case (_,false) => (this,false)
    }
  }
}

/**
 * the duretion between the start of history (time zero) and the curret position in time
 */
case class CurrentTime() extends DoubleExpr(false){
  override def updatedValue(time:Double): Double = time

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    CurrentTime()

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (DoubleExpr, Boolean) = (this,false)
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    Mult(doubleMap(a),doubleMap(b))

  override def cloneExpr : (DoubleExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (Mult(a._1,b._1),true)
  }

}

/**
 * a + b
 * @param a a double expression
 * @param b a double expression
 */
case class Plus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value + b.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    Plus(doubleMap(a),doubleMap(b))

  override def cloneExpr : (DoubleExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (Plus(a._1,b._1),true)
  }
}

case class SumAll(a:DoubleExpr*) extends DoubleExpr(false,a:_*){
  override def updatedValue(time:Double): Double = a.foldLeft(0.0)({case (acc,term) => acc + term.value})
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    SumAll(a.map(doubleMap(_)):_*)

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (DoubleExpr, Boolean) = {
    val clones = a.map(_.cloneExpr)
    val anyRealClone = clones.exists(_._2)
    if(anyRealClone) (SumAll(clones.map(_._1):_*),true)
    else (this,false)
  }
}

/**
 * a - b
 * @param a a double expression
 * @param b a double expression
 */
case class Minus(a:DoubleExpr,b:DoubleExpr) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = a.value - b.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    Minus(doubleMap(a),doubleMap(b))

  override def cloneExpr : (DoubleExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (Minus(a._1,b._1),true)
  }
}

/**
 * if (b == 0) defaultValueIfDividerIsZero else a / b
 * if defaultValueIfDividerIsZero is Nan, throws an exception in case of divide by zero
 * @param a a double expression
 * @param b a double expression
 * @param defaultValueIfDividerIsZero the default value for this if b is zero
 */
case class Div(a:DoubleExpr,b:DoubleExpr,defaultValueIfDividerIsZero:Double = Double.NaN) extends DoubleExpr(false,a,b){
  override def updatedValue(time:Double): Double = if(b.value == 0 && !defaultValueIfDividerIsZero.isNaN) defaultValueIfDividerIsZero else a.value / b.value

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess])=
    Div(doubleMap(a),doubleMap(b),defaultValueIfDividerIsZero)

  override def cloneExpr : (DoubleExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (Div(a._1,b._1,defaultValueIfDividerIsZero),true)
  }
}

/**
 * a > b
 * @param a a double expression
 * @param b a double expression
 */
case class G(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b) {
  override def updatedValue(time:Double): Boolean = a.value > b.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]) =
    G(doubleMap(a),doubleMap(b))

  override def cloneExpr : (BoolExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (G(a._1,b._1),true)
  }
}

/**
 * a >= b
 * @param a a double expression
 * @param b a double expression
 */
case class GE(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value >= b.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]) =
    GE(doubleMap(a),doubleMap(b))

  override def cloneExpr : (BoolExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (GE(a._1,b._1),true)
  }
}

/**
 * a == b
 * @param a a double expression
 * @param b a double expression
 */
case class EQ(a:DoubleExpr,b:DoubleExpr) extends BoolExpr(false,a,b){
  override def updatedValue(time:Double): Boolean = a.value == b.value
  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]) =
    EQ(doubleMap(a),doubleMap(b))

  override def cloneExpr : (BoolExpr, Boolean) = (a.cloneExpr,b.cloneExpr) match{
    case ((ff,false),(gg,false)) => (this,false)
    case (a,b) => (EQ(a._1,b._1),true)
  }
}

case class DoubleITE(i:BoolExpr, t:DoubleExpr, e:DoubleExpr) extends DoubleExpr(false,i,t,e){
  override def updatedValue(time: Double): Double = if(i.value) t.value else e.value

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    DoubleITE(boolMap(i),doubleMap(t),doubleMap(e))

  override def cloneExpr : (DoubleExpr, Boolean) = (i.cloneExpr,t.cloneExpr,e.cloneExpr) match{
    case ((ii,false),(tt,false),(ee,false)) => (this,false)
    case (ii,tt,ee) => (DoubleITE(ii._1,tt._1,ee._1),true)
  }


}

case class BoolSubExpression(name:String,expr:BoolExpr) extends BoolExpr(false,expr){
  override def updatedValue(time: Double): Boolean = expr.value

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): BoolExpr =
    BoolSubExpression(name,boolMap(expr))

  /**
   *
   * @return the clone, true if it is cloned, false if not cloned because nothing modified
   */
  override def cloneExpr : (BoolExpr, Boolean) = expr.cloneExpr match{case (e,true) => (BoolSubExpression(name,e),true) case _ => (this,false)}
}

case class DoubleSubExpression(name:String,expr:DoubleExpr) extends DoubleExpr(false,expr){
  override def updatedValue(time: Double): Double = expr.value

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]): DoubleExpr =
    DoubleSubExpression(name,doubleMap(expr))

  override def cloneExpr : (DoubleExpr, Boolean) = expr.cloneExpr match{case (e,true) => (DoubleSubExpression(name,e),true) case _ => (this,false)}
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess]) =
    PonderateWithDuration(doubleMap(s))

  override def cloneExpr : (DoubleExpr, Boolean) = s.cloneExpr match{case (e,true) => (PonderateWithDuration(e),true) case _ => (this,false)}
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

  override def cloneReset(boolMap: Map[BoolExpr, BoolExpr], doubleMap: Map[DoubleExpr, DoubleExpr], storeMap: Map[Storage, Storage], processMap: Map[ActivableProcess, ActivableProcess])=
    MaxOnHistory(doubleMap(s),if (when == null) null else boolMap(when))


  override def cloneExpr : (DoubleExpr, Boolean) = if(when == null){
    s.cloneExpr match{case (e,true) => (MaxOnHistory(e),true) case _ => (this,false)}
  }else{
    (s.cloneExpr,when.cloneExpr) match{
      case ((ff,false),(gg,false)) => (this,false)
      case (a,b) => (MaxOnHistory(a._1,b._1),true)
    }
  }
}


