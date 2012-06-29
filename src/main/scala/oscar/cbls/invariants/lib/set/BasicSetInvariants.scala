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

/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.invariants.lib.set

import oscar.cbls.invariants.core.computation._;
import collection.immutable.SortedSet;
import collection.immutable.SortedMap;


/** left UNION right
 * @param left is an intvarset
 * @param right is an intvarset
 * */
case class Union(left:IntSetVar, right:IntSetVar) extends IntSetInvariant {
  assert(left != right)
  var output:IntSetVar = null

  def MyMax = left.getMaxVal.max(right.getMaxVal)
  def MyMin = left.getMinVal.min(right.getMinVal)

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  override def setOutputVar(v:IntSetVar){
      output = v
      output.setDefiningInvariant(this)
      output := left.union(right.getValue())
  }

  @inline
  override def notifyInsertOn(v:IntSetVar,value:Int){
    assert(left == v ||right == v)
    output.insertValue(value)
  }

  @inline
  override def notifyDeleteOn(v:IntSetVar,value:Int){
    assert(left == v ||right == v)
    if(v == left){
      if (!right.contains(value)){
        output.deleteValue(value)
      }
    }else if(v == right){
      if(!left.contains(value)){
        output.deleteValue(value)
      }
    }else{
      assert(false)
    }
  }

  override def checkInternals(){
    assert(output.getValue().intersect(left.getValue().union(right.getValue())).size == output.getValue().size)
  }
}

/** left INTER right
 * @param left is an intvarset
 * @param right is an intvarset
 * */
case class Inter(left:IntSetVar, right:IntSetVar) extends IntSetInvariant {

  var output:IntSetVar = null

  def MyMax = left.getMaxVal.min(right.getMaxVal)
  def MyMin = left.getMinVal.max(right.getMinVal)

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  override def setOutputVar(v:IntSetVar){
      output = v.asInstanceOf[IntSetVar]
      output.setDefiningInvariant(this)
      output := left.intersect(right.getValue())
  }

  @inline
  override def notifyInsertOn(v:IntSetVar,value:Int){
    if(v == left){
      if (right.contains(value)){
        output.insertValue(value)
      }
    }else if(v == right){
      if(left.contains(value)){
        output.insertValue(value)
      }
    }else{
      assert(false)
    }
  }

  @inline
  override def notifyDeleteOn(v:IntSetVar,value:Int){
    assert(left == v || right == v)
    output.deleteValue(value)
  }

  override def checkInternals(){
    assert(output.getValue().intersect(left.getValue().intersect(right.getValue())).size == output.getValue().size)
  }
}

/** left MINUS right, the set diff operator
 * @param left is the base set
 * @param right is the set that is removed from left
 * */
case class Diff(left:IntSetVar, right:IntSetVar) extends IntSetInvariant  {

  var output:IntSetVar = null
  def MyMax = left.getMaxVal
  def MyMin = left.getMinVal

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  override def setOutputVar(v:IntSetVar){
      output = v.asInstanceOf[IntSetVar]
      output.setDefiningInvariant(this)
      output := left.diff(right)
  }

  @inline
  override def notifyInsertOn(v:IntSetVar,value:Int){
    if(v == left){
      if (!right.contains(value)){
        output.insertValue(value)
      }
    }else if(v == right){
      if(left.contains(value)){
        output.deleteValue(value)
      }
    }else{
      assert(false)
    }
  }

  @inline
  override def notifyDeleteOn(v:IntSetVar,value:Int){
    if(v == left){
      if (!right.getValue().contains(value)){
        output.deleteValue(value)
      }
    }else if(v == right){
      if(left.getValue().contains(value)){
        output.insertValue(value)
      }
    }else{
      assert(false)
    }
  }

  override def checkInternals(){
    assert(output.getValue().intersect(left.getValue().diff(right.getValue())).size == output.getValue().size)
  }
}


/** #(v) (cardinality)
 * @param v is an IntSetVar, the set of integers to count
 */
case class Cardinality(v:IntSetVar) extends IntInvariant {

  def MyMax = v.getMaxVal-v.getMinVal
  def MyMin = 0

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  var output:IntVar = null

  override def setOutputVar(vv:IntVar){
      output = vv.asInstanceOf[IntVar]
      output.setDefiningInvariant(this)
      output := v.getValue().size
  }

  @inline
  override def notifyInsertOn(v:IntSetVar,value:Int){
    assert(v == this.v)
    output :+= 1
  }

  @inline
  override def notifyDeleteOn(v:IntSetVar,value:Int){
    assert(v == this.v)
    output :-= 1
  }

  override def checkInternals(){
    assert(output.getValue() == v.getValue().size)
  }
}

/** makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
 * @param on is a set of IntVar
 * */
case class MakeSet(on:SortedSet[IntVar]) extends IntSetInvariant {

   var output:IntSetVar = null
   var counts:SortedMap[Int,Int]=on.foldLeft(SortedMap.empty[Int,Int])((acc,intvar) => acc + ((intvar.getValue(),acc.getOrElse(intvar.getValue(),0)+1)))

  for(v <- on) registerStaticAndDynamicDependency(v)
  finishInitialization()
  
  def MyMax = Int.MaxValue
  def MyMin = Int.MinValue

  override def setOutputVar(v:IntSetVar){
      output = v
      output.setDefiningInvariant(this)
      output.setValue(SortedSet.empty[Int] ++ counts.keySet)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    assert(on.contains(v),"MakeSet notified for non interesting var")
    assert(OldVal != NewVal)
    if(counts(OldVal) == 1){
      //on va en supprimer un
      counts = counts - OldVal
      output.deleteValue(OldVal)
    }else{
      //on en supprime pas un
      counts = counts + ((OldVal,counts(OldVal)-1))
    }
    if(counts.contains(NewVal)){
      counts = counts + ((NewVal,counts(NewVal)+1))
    }else{
      counts = counts + ((NewVal,1))
      output.insertValue(NewVal)
    }
  }

  override def checkInternals(){
    assert(output.getValue().size == on.size)
    for(v <- on) assert(output.contains(v.getValue()))
  }
}

/** makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
 * @param lb is the lower bound of the interval
 * @param ub is the upper bound of the interval
 * */
case class Interval(lb:IntVar,ub:IntVar) extends IntSetInvariant {
   assert(ub != lb)
   var output:IntSetVar = null

  def MyMax = ub.MaxVal
  def MyMin = lb.MinVal

  registerStaticAndDynamicDependency(lb)
  registerStaticAndDynamicDependency(ub)
  finishInitialization()

  override def setOutputVar(v:IntSetVar){
      output = v.asInstanceOf[IntSetVar]
      output.setDefiningInvariant(this)
      output.setValue(SortedSet.empty[Int])
      for(i <- lb.getValue() to ub.getValue())output.insertValue(i)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    if(v == lb){
      if(OldVal < NewVal){
        //intervale reduit
        for(i <- OldVal to NewVal-1) output.deleteValue(i)
      }else{
        //intervale plus grand
        for(i <- NewVal to OldVal-1) output.insertValue(i)
      }
    }else{
      if(OldVal > NewVal){
        //intervale reduit
        for(i <- NewVal+1 to OldVal) output.deleteValue(i)
      }else{
        //intervale plus grand
        for(i <- OldVal+1 to NewVal) output.insertValue(i)
      }
    }
  }

   override def checkInternals(){
    assert(output.getValue().size == 0.max(ub.getValue() - lb.getValue() + 1))
     if(ub.getValue() >= lb.getValue()){
       for(i <- lb.getValue() to ub.getValue())
         assert(output.contains(i))
     }
   }
}

