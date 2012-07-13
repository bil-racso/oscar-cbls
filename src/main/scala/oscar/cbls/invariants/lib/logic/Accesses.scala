/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.invariants.lib.logic

import collection.immutable.{SortedSet, SortedMap}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.propagation.KeyForElementRemoval
import oscar.cbls.invariants.core.computation.IntVar._

/** if (ifVar >0) then thenVar else elveVar
 * @param ifVar the condition (IntVar)
 * @param thenVar the returned value if ifVar > 0
 * @param elseVar the returned value if ifVar <= 0
 * */
case class IntITE(ifVar:IntVar, thenVar:IntVar, elseVar:IntVar) extends IntInvariant{

  var output:IntVar = null
  var KeyToCurrentVar:KeyForElementRemoval = null

  registerStaticDependencies(ifVar, thenVar, elseVar)
  registerDeterminingDependency(ifVar)
  KeyToCurrentVar = registerDynamicDependency((if(ifVar.getValue() > 0) thenVar else elseVar))
  finishInitialization()

  def MyMax = thenVar.MaxVal.max(elseVar.MaxVal)
  def MyMin = thenVar.MinVal.min(elseVar.MinVal)


  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    output := (if(ifVar.getValue() > 0) thenVar else elseVar)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    if (v == ifVar){
      if(NewVal > 0 && OldVal <= 0){
        output := thenVar
        //modifier le graphe de dependances
        unregisterDynamicDependency(KeyToCurrentVar)
        KeyToCurrentVar = registerDynamicDependency(thenVar)
      }else if(NewVal <= 0 && OldVal > 0){
        output := elseVar
        //modifier le graphe de dependances
        unregisterDynamicDependency(KeyToCurrentVar)
        KeyToCurrentVar = registerDynamicDependency(elseVar)
      }
    }else{//si c'est justement celui qui est affiche.
      output := NewVal
    }
  }

  override def toString:String= {
    "ITE(" + ifVar + ',' + thenVar + "," + elseVar + ")"
  }

  override def checkInternals(){
    assert(output.getValue() == (if(ifVar.getValue() <= 0) elseVar.getValue() else thenVar.getValue()),this)
  }
}

/** inputarray[index]
 * @param inputarray is an array of IntVar
 * @param index is the index accessing the array*/
case class IntElement(index:IntVar, var inputarray:Array[IntVar])
  extends IntInvariant with Bulked[IntVar,((Int,Int))]{
  var MyMax = 0
  var MyMin = 0

  var output:IntVar = null
  var KeyToCurrentVar:KeyForElementRemoval = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  if(inputarray != null){
    registerStaticDependencyAll(inputarray)
    BulkLoad(inputarray,performBulkComputation(inputarray))
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntVar]):(Int,Int) = {
    val MyMax = bulkedVar.foldLeft(Int.MinValue)((acc,intvar) => if (acc < intvar.MaxVal) intvar.MaxVal else acc)
    val MyMin = bulkedVar.foldLeft(Int.MaxValue)((acc,intvar) => if (acc > intvar.MinVal) intvar.MinVal else acc)
    (MyMin,MyMax)
  }

  override def BulkLoad(bulkedVar: Array[IntVar],bcr:(Int,Int)){
    inputarray = bulkedVar
    KeyToCurrentVar = registerDynamicDependency(inputarray(index))
    MyMin = bcr._1
    MyMax = bcr._2
  }

  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    output := inputarray.apply(index)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    if (v == index){
      output := inputarray(NewVal)
      //modifier le graphe de dependances
      unregisterDynamicDependency(KeyToCurrentVar)
      KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
    }else{//si c'est justement celui qui est affiche.
      assert(v == inputarray.apply(index),"access notified for non listened var")
      output := NewVal
    }
  }

  override def checkInternals(){
    assert(output.getValue() == inputarray(index).getValue(), this)
  }

  override def toString:String= {
    inputarray.toList.toString+"[" + index.toString + "]"
  }
}

/**Union(i in index) (array[i])
 * @param index is an IntSetVar denoting the set of positions in the array to consider
 * @param inputarray is the array of intvar that can be selected by the index
 */
case class IntElements(index:IntSetVar, var inputarray:Array[IntVar])
  extends IntSetInvariant with Bulked[IntVar,((Int,Int))]{
  var MyMax = 0
  var MyMin = 0

  var output:IntSetVar = null
  val KeysToInputArray:Array[KeyForElementRemoval] = new Array(inputarray.size)

  //TODO: this could be exchanged for an array.
  var ValueCount:SortedMap[Int,Int] = index.foldLeft(SortedMap.empty[Int,Int])((acc,i)
  => acc+((inputarray(i).getValue(),acc.getOrElse(inputarray(i),0)+1)))

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  if(inputarray != null){
    registerStaticDependencyAll(inputarray)
    BulkLoad(inputarray,performBulkComputation(inputarray))
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntVar]):(Int,Int) = {
    val MyMax = bulkedVar.foldLeft(Int.MinValue)((acc,intvar) => if (acc < intvar.MaxVal) intvar.MaxVal else acc)
    val MyMin = bulkedVar.foldLeft(Int.MaxValue)((acc,intvar) => if (acc > intvar.MinVal) intvar.MinVal else acc)
    (MyMin,MyMax)
  }

  override def BulkLoad(bulkedVar: Array[IntVar],bcr:(Int,Int)){
    inputarray = bulkedVar
    for(v <- index.getValue()) KeysToInputArray.update(v,registerDynamicDependency(inputarray(v),v))
    MyMin = bcr._1
    MyMax = bcr._2
  }

  override def setOutputVar(v:IntSetVar){
    output = v
    output.setDefiningInvariant(this)
    output := index.foldLeft(SortedSet.empty[Int])((acc:SortedSet[Int],indice:Int) => acc+inputarray(indice))
  }

  @inline
  override def notifyIntChanged(v:IntVar,indice:Int,OldVal:Int,NewVal:Int){
    assert(inputarray(indice) == v)
    if(KeysToInputArray(indice) != null){
      //il etait dans le truc, il faut changer la valeur
      val NewCount = ValueCount(OldVal)-1
      ValueCount+=((OldVal,NewCount))
      if(NewCount == 0) output.deleteValue(OldVal)

      val OtherNewCount = ValueCount.getOrElse(NewVal,0)+1
      ValueCount+=((NewVal,OtherNewCount))
      if(OtherNewCount == 1) output.insertValue(NewVal)
    }
  }

  @inline
  override def notifyInsertOn(v:IntSetVar,value:Int){
    assert(index == v)
    KeysToInputArray.update(value,registerDynamicDependency(inputarray(value)))
    val NewVal:Int = inputarray(value)
    val NewCount:Int = ValueCount.getOrElse(NewVal,0)+1
    ValueCount += ((NewVal,NewCount))
    if(NewCount==1){output.insertValue(inputarray(value))}
  }

  @inline
  override def notifyDeleteOn(v:IntSetVar,value:Int){
    assert(index == v)
    assert(KeysToInputArray(value) != null)
    unregisterDynamicDependency(KeysToInputArray(value))
    KeysToInputArray.update(value,null)
    val OldVal:Int = inputarray(value)
    val NewCount:Int = ValueCount.getOrElse(OldVal,0)-1 //le orelse c'est pour compiler.
    ValueCount+=((OldVal,NewCount))
    if(NewCount==0) output.deleteValue(inputarray(value))
  }

  override def checkInternals(){
    assert(KeysToInputArray.indices.forall(i => ((KeysToInputArray(i) != null) == index.getValue().contains(i))))
    assert(index.getValue().forall((i:Int) =>
      output.getValue().contains(inputarray(i).getValue())), "" + index + inputarray.toList + output)
    assert(output.getValue().size == index.getValue().size)
  }
}

/** inputarray[index] on an array of IntSetVar
 * @param inputarray is the array of intsetvar
 * @param index is the index of the array access
 **/
case class IntSetElement(index:IntVar, var inputarray:Array[IntSetVar])
  extends IntSetInvariant with Bulked[IntSetVar,((Int,Int))]{

  var MyMax = 0
  var MyMin = 0

  var output:IntSetVar = null
  var KeyToCurrentVar:KeyForElementRemoval = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  if(inputarray != null){
    registerStaticDependencyAll(inputarray)
    BulkLoad(inputarray,performBulkComputation(inputarray))
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntSetVar]):(Int,Int) = {
    val MyMax = bulkedVar.foldLeft(Int.MinValue)((acc,intsetvar) => if (acc < intsetvar.getMaxVal) intsetvar.getMaxVal else acc)
    val MyMin = bulkedVar.foldLeft(Int.MaxValue)((acc,intsetvar) => if (acc > intsetvar.getMinVal) intsetvar.getMinVal else acc)
    (MyMin,MyMax)
  }

  override def BulkLoad(bulkedVar: Array[IntSetVar],bcr:(Int,Int)){
    inputarray = bulkedVar
    KeyToCurrentVar = registerDynamicDependency(inputarray(index))
    MyMin = bcr._1
    MyMax = bcr._2
  }


  override def setOutputVar(v:IntSetVar){
    output = v
    output.setDefiningInvariant(this)
    output := inputarray.apply(index)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    assert(v == index)
    output := inputarray(NewVal)
    //modifier le graphe de dependances
    unregisterDynamicDependency(KeyToCurrentVar)
    KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int){
    assert(v == inputarray.apply(index))
    output.deleteValue(value)
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int){
    assert(v == inputarray.apply(index))
    output.insertValue(value)
  }

  override def checkInternals(){
    assert(output.getValue().intersect(inputarray(index.getValue()).getValue()).size == output.getValue().size)
  }
}
