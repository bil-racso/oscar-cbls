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

package oscar.cbls.lib.invariant.logic

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.core.computation.DomainRange

import scala.collection.immutable.SortedSet



/**
 * if (ifVar > pivot) then thenVar else elveVar
 * @param ifVar the condition (IntVar)
 * @param thenVar the returned value if ifVar > pivot
 * @param elseVar the returned value if ifVar <= pivot
 * @author renaud.delandtsheer@cetic.be
 * */
case class IntITE(ifVar: IntValue, thenVar: IntValue, elseVar: IntValue, pivot: Long = 0L)
  extends IntInvariant(if(ifVar.value > pivot) thenVar.value else elseVar.value, thenVar.domain union elseVar.domain)
  with VaryingDependencies with IntNotificationTarget{

  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependencies(ifVar, thenVar, elseVar)
  registerDeterminingDependency(ifVar)
  KeyToCurrentVar = registerDynamicDependency(if (ifVar.value > pivot) thenVar else elseVar)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    if (v == ifVar) {
      if (NewVal > pivot && OldVal <= pivot) {
        //modifier le graphe de dependances
        KeyToCurrentVar.performRemove()
        KeyToCurrentVar = registerDynamicDependency(thenVar)
        this := thenVar.value
      } else if (NewVal <= pivot && OldVal > pivot) {
        //modifier le graphe de dependances
        KeyToCurrentVar.performRemove()
        KeyToCurrentVar = registerDynamicDependency(elseVar)
        this := elseVar.value
      }
    } else { //si c'est justement celui qui est affiche.
      this := NewVal
    }
  }

  override def toString: String = {
    "ITE(" + ifVar + ',' + thenVar + "," + elseVar + ")"
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == (if (ifVar.value <= pivot) elseVar.value else thenVar.value),
      Some("output.value (" + this.value
        + ") == (if (ifVar.value (" + ifVar.value + ") <= " + pivot + ") elseVar.value (" + elseVar.value
        + ") else thenVar.value (" + thenVar.value + "))"))
  }
}

/**
 * inputarray[index]
 * @param inputArray is an array of Long
 * @param index is the index accessing the array
 * @author renaud.delandtsheer@cetic.be
 * */
case class ConstantIntElement(index: IntValue, inputArray: Array[Long])
  extends Int2Int(index, inputArray(_), InvariantHelper.getMinMaxBoundsInt(inputArray))

/**
 * inputarray[index]
 * @param inputarray is an array of IntVar
 * @param index is the index accessing the array
 * @author renaud.delandtsheer@cetic.be
 * */
case class IntElement(index: IntValue, inputarray: Array[IntValue])
  extends IntInvariant(initialValue = inputarray(index.value).value)
  with Bulked[IntValue, Domain]
  with VaryingDependencies
  with IntNotificationTarget{

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  restrictDomain(bulkRegister(inputarray))

  var KeyToCurrentVar = registerDynamicDependency(inputarray(index.value))

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntValue]): Domain = {
    InvariantHelper.getMinMaxBounds(bulkedVar)
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    if (v == index) {
      //modifier le graphe de dependances
      KeyToCurrentVar.performRemove()
      KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
      this := inputarray(NewVal).value
    } else { //si c'est justement celui qui est affiche.
      assert(v == inputarray.apply(index.value), "access notified for non listened var")
      this := NewVal
    }
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == inputarray(index.value).value,
      Some("output.value (" + this.value + ") == inputarray(index.value ("
        + index.value + ")).value (" + inputarray(index.value).value + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4L){
      "Array(" +inputs.take(4L).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}

/**
 * inputarray[index]
 * @param inputarray is an array of Long
 * @param index is the index accessing the array (its value must be always inside the array range)
 * @author renaud.delandtsheer@cetic.be
 * @author jean-noel.monette@it.uu.se
 * */
case class IntElementNoVar(index: IntValue, inputarray: Array[Long])
  extends IntInvariant(initialValue = inputarray(index.value),DomainRange(inputarray.min,inputarray.max))
  with IntNotificationTarget{

  registerStaticAndDynamicDependency(index)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
   // println(OldVal + " "+ NewVal)
    if(NewVal >= inputarray.length){
      println("something is wrong")
    }
    this := inputarray(NewVal)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == inputarray(index.value),
      Some("output.value (" + this.value + ") == inputarray(index.value ("
        + index.value + ")) (" + inputarray(index.value) + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4L){
      "Array(" +inputs.take(4L).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "] = " + this.value
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "] = " + this.value
    }
  }
}

/**
 * Union(i in index) (array[i])
 * @param index is an IntSetVar denoting the set of positions in the array to consider
 * @param inputarray is the array of intvar that can be selected by the index
 * @author renaud.delandtsheer@cetic.be
 * */
case class Elements[T <:IntValue](index: SetValue, inputarray: Array[T])
  extends SetInvariant
  with Bulked[T, Domain]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  val KeysToInputArray: Array[KeyForElementRemoval] = new Array(inputarray.length)

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  restrictDomain(bulkRegister(inputarray))

  for (v <- index.value) KeysToInputArray(v) = registerDynamicDependency(inputarray(v), v)

  finishInitialization()

  //this array is the number of elements with value i-myMin
  var ValueCount: Array[Long] = Array.tabulate(max-min+1L)(_ => 0L)

  for (arrayPosition <- index.value) {
    val value = inputarray(arrayPosition).value
    internalInsert(value)
  }

  override def performBulkComputation(bulkedVar: Array[T]): Domain =
    InvariantHelper.getMinMaxBounds(bulkedVar)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    internalDelete(OldVal)
    internalInsert(NewVal)
  }


  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    assert(index == v)
    for(value <- addedValues){
      KeysToInputArray(value) = registerDynamicDependency(inputarray(value))
      val NewVal: Long = inputarray(value).value

      internalInsert(NewVal)
    }

    for(value <- removedValues){
      assert(KeysToInputArray(value) != null)

      KeysToInputArray(value).performRemove()
      KeysToInputArray(value) = null

      val OldVal:Long = inputarray(value).value
      internalDelete(OldVal)
    }
  }

  private def internalInsert(value:Long){
    if (ValueCount(value - min) == 0L){
      ValueCount(value - min) = 1L
      this :+= value
    }else{
      ValueCount(value - min) += 1L
    }
    assert(ValueCount(value - min) > 0L)
  }

  private def internalDelete(value:Long){
    assert(ValueCount(value - min) > 0L)
    if (ValueCount(value - min) == 1L){
      ValueCount(value - min) = 0L
      this :-= value
    }else{
      ValueCount(value - min) -= 1L
    }
  }

  override def checkInternals(c: Checker) {
    c.check(KeysToInputArray.indices.forall(i => (KeysToInputArray(i) != null) == index.value.contains(i)),
      Some("KeysToInputArray.indices.forall(i => ((KeysToInputArray(i) != null) == index.value.contains(i)))"))
    c.check(index.value.forall((i: Long) =>
      this.value.contains(inputarray(i).value)),
      Some("index.value.forall((i: Long) => output.value.contains(inputarray(i).value))"))
    c.check(this.value.size <= index.value.size,
      Some("output.value.size (" + this.value.size + ") <= index.value.size (" + index.value.size + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4L){
      "Array(" +inputs.take(4L).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
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
case class SetElement[X<:SetValue](index: IntValue, inputarray: Array[X])
  extends SetInvariant(inputarray.apply(index.value).value)
  with Bulked[X, Domain]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  restrictDomain(bulkRegister(inputarray))

  KeyToCurrentVar = registerDynamicDependency(inputarray(index.value))

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[X]): Domain =
    InvariantHelper.getMinMaxBoundsSet(bulkedVar)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    assert(v == index)
    //modifier le graphe de dependances
    KeyToCurrentVar.performRemove()
    KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
    this := inputarray(NewVal).value
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    assert(v == inputarray.apply(index.value))
    this := newValue
  }

  override def checkInternals(c: Checker) {
    c.check(this.value.intersect(inputarray(index.value).value).size == this.value.size,
      Some("output.value.intersect(inputarray(index.value (" + index.value + ")).value ("
        + inputarray(index.value).value + ")).size ("
        + this.value.intersect(inputarray(index.value).value).size
        + ") == output.value.size (" + this.value.size + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4L){
      "Array(" +inputs.take(4L).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}
