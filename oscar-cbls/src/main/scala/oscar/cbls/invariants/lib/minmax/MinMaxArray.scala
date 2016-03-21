/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Yoann Guyot
  ******************************************************************************/

package oscar.cbls.invariants.lib.minmax

import oscar.cbls.invariants.core.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, KeyForElementRemoval}

import scala.collection.immutable.SortedSet

/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxArray(varss: Array[IntValue], cond: SetValue = null, default: Int = Int.MinValue)
  extends MiaxArray(varss, cond, default) {

  override def Ord(v: IntValue): Int = -v.value

  override def ExtremumName: String = "Max"

  //More precise bounds
  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    if (cond == null){
      (bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.min > acc) intvar.min else acc),
        bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.max > acc) intvar.max else acc))
    }else super.performBulkComputation(bulkedVar)

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value >= v.value,
        Some("output.value (" + this.value + ") >= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinArray(varss: Array[IntValue], cond: SetValue = null, default: Int = Int.MaxValue)
  extends MiaxArray(varss, cond, default) {

  override def Ord(v: IntValue): Int = v.value

  override def ExtremumName: String = "Min"

  //More precise bounds
  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    if (cond == null){
      (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.min < acc) intvar.min else acc),
        bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.max < acc) intvar.max else acc))
    }else super.performBulkComputation(bulkedVar)

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value <= v.value,
        Some("this.value (" + this.value + ") <= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxArray(vars: Array[IntValue], cond: SetValue, default: Int)
  extends IntInvariant with Bulked[IntValue, Domain]
  with VaryingDependencies
  with IntNotificationTarget{

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.length)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  /**
   * since the value of the bulkcomputationResult depends on the presence or absence of cond,
   * we register two bcr, so that you can join the correct bulk whataver happens.
   */
  restrictDomain(bulkRegister(vars,if (cond == null) 0 else 1).union(default))

  if (cond != null) {
    for (i <- cond.value) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  } else {
    for (i <- vars.indices) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    InvariantHelper.getMinMaxBounds(bulkedVar)

  def ExtremumName: String
  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst).value
    }
  }
}




/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of Int
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinConstArray(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue)
  extends MiaxConstArray(varss, ccond, default) {

  override def Ord(v: Int): Int = v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default)
    else  c.check(value == ccond.value.minBy(varss(_)))
  }
}


/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxConstArray(varss: Array[Int], ccond: SetValue, default: Int = Int.MinValue)
  extends MiaxConstArray(varss, ccond, default) {

  override def Ord(v: Int): Int = -v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default)
    else  c.check(value == ccond.value.maxBy(varss(_)))
  }
}


/**
 * Maintains Min(Var(i) | i in cond)
 * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
 * postponed updates are ones that do not impact on the outout of the invariant.
 * when there is an update, it is first checked against the TODO-list, for cancellation.
 * if the update does not impact the output, it is postponed
 * if it affects the output, it is performed
 * @param varss is an array of Int
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param default the value if ccond is empty
 * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
 * update is O(log(n)), faster (O(1) if you do updates and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinConstArrayLazy(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue, maxBackLogSize:Int = Int.MaxValue)
  extends MiaxConstArrayLazy(varss, ccond, default, maxBackLogSize) {

  override def Ord(v: Int): Int = v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default)
    else  c.check(value == ccond.value.minBy(varss(_)))
  }

  @inline
  override def equalOrNotImpactingMiax(potentialMiax: Int): Boolean = this.getValue(true) <= potentialMiax
}


/**
 * Maintains Max(Var(i) | i in cond)
 * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
 * postponed updates are ones that do not impact on the outout of the invariant.
 * when there is an update, it is first checked against the TODO-list, for cancellation.
 * if the update does not impact the output, it is postponed
 * if it affects the output, it is performed
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param default the value if ccond is empty
 * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
 * update is O(log(n)), faster (O(1) if you do updates and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxConstArrayLazy(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue, maxBackLogSize:Int = 10)
  extends MiaxConstArrayLazy(varss, ccond, default, maxBackLogSize) {

  @inline
  override def Ord(v: Int): Int = -v

  override def checkInternals(c: Checker): Unit = {
    if(ccond.value.isEmpty) c.check(value == default)
    else  c.check(value == ccond.value.maxBy(varss(_)))
  }

  @inline
  override def equalOrNotImpactingMiax(potentialMiax: Int): Boolean = this.getValue(true) >= potentialMiax
}


/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArray(vars: Array[Int], cond: SetValue, default: Int)
  extends IntInvariant{

  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  //TODO: restrict domain

  for (i <- cond.value) {
    h.insert(i)
  }

  def Ord(v: Int): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst)
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst)
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst)
    }
  }

  override def checkInternals(c: Checker): Unit = {
    if(cond.value.isEmpty) c.check(value == default)
    else  c.check(value == cond.value.maxBy(vars(_)))
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n)), but probably faster if you do neighborhood exploration with moves and backtracks
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArrayLazy(vars: Array[Int], cond: SetValue, default: Int, maxBacklog:Int = Int.MaxValue)
  extends IntInvariant{

//  var nbAnihilation = 0
//  var nbDoIt = 0

  val n = vars.length
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  var backLog:QList[Int] = null
  var backlogSize:Int = 0
  val isBacklogged:Array[Boolean] = Array.fill(vars.size)(false)
  val consideredValue:Array[Boolean] = Array.fill(vars.size)(false)

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  def computeMinMax():(Int,Int) = {
    var myMin = default
    var myMax = default
    for (i <- vars) {
      if(i > myMax) myMax = i
      if(i < myMin) myMin = i
    }
    (myMin,myMax)
  }

  restrictDomain(computeMinMax())

  for (i <- cond.value) {
    h.insert(i)
    consideredValue(i) = true
  }

  def Ord(v: Int): Int

  @inline
  private[this] def updateFromHeap() {
    if (h.isEmpty) {
      this := default
    } else {
      updateFromNonEmptyHeap()
    }
  }

  @inline
  private[this] def updateFromNonEmptyHeap() {
    this := vars(h.getFirst)
  }

  updateFromHeap()

  @inline
  def equalOrNotImpactingMiax(potentialMiax:Int):Boolean

  @inline
  private[this] def putIntoBackLog(cond:Int): Unit ={
    if(!isBacklogged(cond)){
      backLog = QList(cond,backLog)
      isBacklogged(cond) = true
      backlogSize += 1
    }
  }

  /**
   * does not perform final update because was not supposed to be impacted
   */
  @inline
  private[this] def trimBackLog(){

/*    def quickTrim(l:QList[Int]):QList[Int] = {
      if(l == null) null
      else{
        if(isBacklogged(l.head)){
          QList(l.head,quickTrim(l.tail))
        }else {
          backlogSize -= 1
          quickTrim(l.tail)
        }
      }
    }

    backLog = quickTrim(backLog)*/

    while(true){
      if(backLog == null) return
      if(!isBacklogged(backLog.head)){
        backlogSize -=1
        backLog = backLog.tail
      } else if(backlogSize > maxBacklog){
        val condValue = backLog.head
        backlogSize -=1
        backLog = backLog.tail
        processThisRealBackLog(condValue)
      }else{
        return
      }
    }
  }

  @inline
  private[this] def processThisRealBackLog(condValue:Int): Unit ={
    if(consideredValue(condValue)){ //should be removed
      assert(!cond.value.contains(condValue))
      h.delete(condValue)
      consideredValue(condValue) = false
//      nbDoIt +=1
    }else{ //should be added
      assert(cond.value.contains(condValue))
      h.insert(condValue)
      consideredValue(condValue) = true
//      nbDoIt +=1
    }
    isBacklogged(condValue) = false
  }


  @inline
  private[this] def processBackLog(): Unit ={
    while(backLog != null){
      val condValue = backLog.head
      backLog = backLog.tail
      if(isBacklogged(condValue)) {
        processThisRealBackLog(condValue)
      }
    }
    backlogSize = 0
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    if(consideredValue(value)){ //anihilation
      assert(isBacklogged(value))
      isBacklogged(value) = false
//      nbAnihilation += 2
      return
    }
    if(equalOrNotImpactingMiax(vars(value))){//backlog
      trimBackLog()
      putIntoBackLog(value)
    }else{//impacted
      this := vars(value)
      h.insert(value)
//      nbDoIt +=1
      consideredValue(value) = true
    }
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    if(!consideredValue(value)){ //anihilation
      assert(isBacklogged(value))
      isBacklogged(value) = false
//      nbAnihilation += 2
      return
    }
    if(this.getValue(true) == vars(value)){//impacted, flush backLog
      processBackLog()
      h.delete(value)
//      nbDoIt +=1
      consideredValue(value) = false
      updateFromHeap()
    }else{//not impacted, backlog
      trimBackLog()
      putIntoBackLog(value)
    }
  }


}
