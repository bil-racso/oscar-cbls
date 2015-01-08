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
  * ****************************************************************************
  */

package oscar.cbls.invariants.lib.logic

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{ Checker, KeyForElementRemoval }
import scala.collection.immutable.Set

/**
 * if (ifVar >0) then thenVar else elveVar
 * @param ifVar the condition (IntVar)
 * @param thenVar the returned value if ifVar > 0
 * @param elseVar the returned value if ifVar <= 0
 * @author renaud.delandtsheer@cetic.be
 * */
case class IntITE(ifVar: CBLSIntVar, thenVar: CBLSIntVar, elseVar: CBLSIntVar) extends IntInvariant with VaryingDependenciesInvariant {

  var output: CBLSIntVar = null
  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependencies(ifVar, thenVar, elseVar)
  registerDeterminingDependency(ifVar)
  KeyToCurrentVar = registerDynamicDependency((if (ifVar.value > 0) thenVar else elseVar))
  finishInitialization()

  def myMax = thenVar.maxVal.max(elseVar.maxVal)
  def myMin = thenVar.minVal.min(elseVar.minVal)

  override def setOutputVar(v: CBLSIntVar) {
    output = v
    output.setDefiningInvariant(this)
    output := (if (ifVar.value > 0) thenVar else elseVar).value
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    if (v == ifVar) {
      if (NewVal > 0 && OldVal <= 0) {
        //modifier le graphe de dependances
        unregisterDynamicDependency(KeyToCurrentVar)
        KeyToCurrentVar = registerDynamicDependency(thenVar)
        output := thenVar.value
      } else if (NewVal <= 0 && OldVal > 0) {
        //modifier le graphe de dependances
        unregisterDynamicDependency(KeyToCurrentVar)
        KeyToCurrentVar = registerDynamicDependency(elseVar)
        output := elseVar.value
      }
    } else { //si c'est justement celui qui est affiche.
      output := NewVal
    }
  }

  override def toString: String = {
    "ITE(" + ifVar + ',' + thenVar + "," + elseVar + ")"
  }

  override def checkInternals(c: Checker) {
    c.check(output.value == (if (ifVar.value <= 0) elseVar.value else thenVar.value),
      Some("output.value (" + output.value
        + ") == (if (ifVar.value (" + ifVar.value + ") <= 0) elseVar.value (" + elseVar.value
        + ") else thenVar.value (" + thenVar.value + "))"))
  }
}

/**
 * inputarray[index]
 * @param inputarray is an array of IntVar
 * @param index is the index accessing the array
 * @author renaud.delandtsheer@cetic.be
 * */
case class IntElement(index: CBLSIntVar, inputarray: Array[CBLSIntVar])
  extends IntInvariant with Bulked[CBLSIntVar, ((Int, Int))] with VaryingDependenciesInvariant {

  var output: CBLSIntVar = null
  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  val (myMin, myMax) = bulkRegister(inputarray)

  KeyToCurrentVar = registerDynamicDependency(inputarray(index.value))

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[CBLSIntVar]): (Int, Int) = {
    InvariantHelper.getMinMaxBounds(bulkedVar)
  }

  override def setOutputVar(v: CBLSIntVar) {
    output = v
    output.setDefiningInvariant(this)
    output := inputarray.apply(index.value).value
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    if (v == index) {
      //modifier le graphe de dependances
      unregisterDynamicDependency(KeyToCurrentVar)
      KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
      output := inputarray(NewVal).value
    } else { //si c'est justement celui qui est affiche.
      assert(v == inputarray.apply(index.value), "access notified for non listened var")
      output := NewVal
    }
  }

  override def checkInternals(c: Checker) {
    c.check(output.value == inputarray(index.value).value,
      Some("output.value (" + output.value + ") == inputarray(index.value ("
        + index.value + ")).value (" + inputarray(index.value).value + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}

/**
 * Union(i in index) (array[i])
 * @param index is an IntSetVar denoting the set of positions in the array to consider
 * @param inputarray is the array of intvar that can be selected by the index
 * @author renaud.delandtsheer@cetic.be
 * */
case class Elements(index: CBLSSetVar, inputarray: Array[CBLSIntVar])
  extends SetInvariant with Bulked[CBLSIntVar, ((Int, Int))] with VaryingDependenciesInvariant {

  var output: CBLSSetVar = null
  val KeysToInputArray: Array[KeyForElementRemoval] = new Array(inputarray.size)

  //this array is the number of elements with value i-myMin
  var ValueCount: Array[Int] = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  val (myMin, myMax) = bulkRegister(inputarray)
  for (v <- index.value) KeysToInputArray(v) = registerDynamicDependency(inputarray(v), v)

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[CBLSIntVar]): (Int, Int) =
    InvariantHelper.getMinMaxBounds(bulkedVar)

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)

    ValueCount = Array.tabulate(myMax - myMin + 1)(_ => 0)

    output := SortedSet.empty
    for (arrayPosition <- index.value) {
      val value = inputarray(arrayPosition).value
      internalInsert(value)
    }
  }

  @inline
  override def notifyIntChanged(v:CBLSIntVar,OldVal:Int,NewVal:Int){
    internalDelete(OldVal)
    internalInsert(NewVal)
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    assert(index == v)
    KeysToInputArray(value) = registerDynamicDependency(inputarray(value))
    val NewVal: Int = inputarray(value).value

    internalInsert(NewVal)
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(index == v)
    assert(KeysToInputArray(value) != null)

    unregisterDynamicDependency(KeysToInputArray(value))
    KeysToInputArray(value) = null

    val OldVal:Int = inputarray(value).value
    internalDelete(OldVal)

  }

  private def internalInsert(value:Int){
    if (ValueCount(value - myMin) == 0){
      ValueCount(value - myMin) = 1
      output :+= value
    }else{
      ValueCount(value - myMin) += 1
    }
    assert(ValueCount(value - myMin) > 0)
  }

  private def internalDelete(value:Int){
    assert(ValueCount(value - myMin) > 0)
    if (ValueCount(value - myMin) == 1){
      ValueCount(value - myMin) = 0
      output :-= value
    }else{
      ValueCount(value - myMin) -= 1
    }
  }

  override def checkInternals(c: Checker) {
    c.check(KeysToInputArray.indices.forall(i => ((KeysToInputArray(i) != null) == index.value.contains(i))),
      Some("KeysToInputArray.indices.forall(i => ((KeysToInputArray(i) != null) == index.value.contains(i)))"))
    c.check(index.value.forall((i: Int) =>
      output.value.contains(inputarray(i).value)),
      Some("index.value.forall((i: Int) => output.value.contains(inputarray(i).value))"))
    c.check(output.value.size <= index.value.size,
      Some("output.value.size (" + output.value.size + ") <= index.value.size (" + index.value.size + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}

/**
 * inputarray[index] on an array of IntSetVar
 * @param inputarray is the array of intsetvar
 * @param index is the index of the array access
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetElement(index: CBLSIntVar, inputarray: Array[CBLSSetVar])
  extends SetInvariant with Bulked[CBLSSetVar, ((Int, Int))] with VaryingDependenciesInvariant {

  var output: CBLSSetVar = null
  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  val (myMin, myMax) = bulkRegister(inputarray)

  KeyToCurrentVar = registerDynamicDependency(inputarray(index.value))

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[CBLSSetVar]): (Int, Int) =
    InvariantHelper.getMinMaxBoundsIntSetVar(bulkedVar)

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output := inputarray.apply(index.value).value
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    assert(v == index)
    //modifier le graphe de dependances
    unregisterDynamicDependency(KeyToCurrentVar)
    KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
    output := inputarray(NewVal).value
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(v == inputarray.apply(index.value))
    output.deleteValue(value)
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    assert(v == inputarray.apply(index.value))
    output.insertValue(value)
  }

  override def checkInternals(c: Checker) {
    c.check(output.value.intersect(inputarray(index.value).value).size == output.value.size,
      Some("output.value.intersect(inputarray(index.value (" + index.value + ")).value ("
        + inputarray(index.value).value + ")).size ("
        + output.value.intersect(inputarray(index.value).value).size
        + ") == output.value.size (" + output.value.size + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}
