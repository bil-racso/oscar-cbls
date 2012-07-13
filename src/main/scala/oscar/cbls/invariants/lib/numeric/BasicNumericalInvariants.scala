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

package oscar.cbls.invariants.lib.numeric


import collection.immutable.SortedSet;
import collection.immutable.SortedMap;
import oscar.cbls.invariants.core.computation.Implicits._;
import oscar.cbls.invariants.core.computation._;

import oscar.cbls.invariants.lib.logic._;

/** sum(vars)
 * @param vars is an iterable of IntVars
 * */
case class Sum(vars:Iterable[IntVar]) extends IntInvariant {
  assert(vars.size>0,"Invariant plus declared with zero vars to sum up")

  for(v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  def MyMin = vars.foldLeft(0)((acc,intvar) => acc + intvar.MinVal)
  def MyMax = vars.foldLeft(0)((acc,intvar) => acc + intvar.MaxVal)

  var output:IntVar = null

  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    output := vars.foldLeft(0)((a,b) => a+b)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    output := output.getValue(true) + NewVal - OldVal
  }

  override def checkInternals(){
    assert(output.getValue() == vars.foldLeft(0)((acc,intvar) => acc+intvar.getValue()))
  }
}

/** prod(vars)
 * @param vars is a set of IntVars
 * */
case class Prod(vars:Iterable[IntVar]) extends IntInvariant {
  assert(vars.size>0,"Invariant prod declared with zero vars to multiply")

  for(v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  var NullVarCount:Int=vars.count(v => v.getValue() == 0)
  var NonNullProd:Int = vars.foldLeft(1)((acc,intvar) => if(intvar.getValue() == 0){acc}else{acc*intvar})

  var output:IntVar = null

  //TODO: find better bound, this is far too much
  def MyMax = vars.foldLeft(1)((acc,intvar) => acc * (if(intvar.MaxVal > -intvar.MinVal) intvar.MaxVal else -intvar.MinVal))
  def MyMin = - MyMax


  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    if (NullVarCount != 0){
      output := 0
    }else{
      output := NonNullProd
    }
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    assert(OldVal != NewVal)
    if (OldVal == 0 && NewVal != 0){
      NullVarCount -=1
      NonNullProd *=NewVal
    }else if(OldVal != 0 && NewVal == 0){
      NullVarCount +=1
      NonNullProd =NonNullProd/OldVal
    }else{
      NonNullProd = NonNullProd/OldVal
      NonNullProd = NonNullProd * NewVal
    }
    if (NullVarCount == 0){
      output := NonNullProd
    }else{
      output := 0
    }
  }

  override def checkInternals(){
    var prod = 1;
    for (v <- vars) prod *= v.getValue()
    assert(output.getValue() == prod)
  }
}

/** left - right
 * where left, right, and output are IntVar*/
case class Minus(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, ((l:Int, r:Int) => l-r), left.MinVal - right.MaxVal, left.MaxVal - right.MinVal){
  assert(left != right)
}

/** left + right
 * where left, right, and output are IntVar*/
case class Sum2(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, ((l:Int, r:Int) => l+r), left.MinVal + right.MinVal, left.MaxVal + right.MaxVal)

/** left * right
 * where left, right, and output are IntVar*/
case class Prod2(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, ((l:Int, r:Int) => l*r), Int.MinValue, Int.MaxValue)

/**left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual... */
case class Div(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, (l:Int, r:Int) => l/r)

/**abs(v) (absolute value)
 * where output and v are IntVar*/
case class Abs(v:IntVar)
  extends IntVar2IntVarFun(v, ((x:Int) => x.abs), (if(v.MinVal<=0)0 else v.MinVal), v.MaxVal.max(-v.MinVal))

/**
 * This invariant implements a step function. Values higher than pivot are mapped to ifval
 * values lower or equal to pivot are mapped to elseval
 * This invariant was suggested by Jean-Noël Monette
 *
 * @param x the IntVar parameter of the invariant
 * @param pivot the pivot value
 * @param thenval the value returned when x > pivot
 * @param elseval the value returned when x <= pivot
 */
case class Step(x:IntVar,pivot:Int = 0,thenval:Int = 1,elseval:Int = 0)
  extends IntVar2IntVarFun(x, (a:Int) => if (a>pivot) thenval else elseval,0,1)
