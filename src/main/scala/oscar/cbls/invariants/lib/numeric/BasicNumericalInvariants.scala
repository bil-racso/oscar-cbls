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
 ******************************************************************************/

package oscar.cbls.invariants.lib.numeric



import oscar.cbls.invariants.core.computation._;

import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.core.propagation.checker
;

/** sum(vars)
 * @param vars is an iterable of IntVars
 * */
case class Sum(vars:Iterable[IntVar]) extends IntInvariant {
  assert(vars.size>0,"Invariant + declared with zero vars to sum up")

  for(v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  def myMin = vars.foldLeft(0)((acc,intvar) => acc + intvar.minVal)
  def myMax = vars.foldLeft(0)((acc,intvar) => acc + intvar.maxVal)

  var output:IntVar = null

  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    output := vars.foldLeft(0)((a,b) => a+b.value)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    output :+= NewVal - OldVal
  }

  override def checkInternals(c:checker){
    c.check(output.value == vars.foldLeft(0)((acc,intvar) => acc+intvar.value))
  }
}

/** prod(vars)
 * @param vars is a set of IntVars
 * */
case class Prod(vars:Iterable[IntVar]) extends IntInvariant {
  assert(vars.size>0,"Invariant prod declared with zero vars to multiply")

  for(v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  var NullVarCount:Int=vars.count(v => v.value == 0)
  var NonNullProd:Int = vars.foldLeft(1)((acc,intvar) => if(intvar.value == 0){acc}else{acc*intvar.value})

  var output:IntVar = null

  //TODO: find better bound, this is far too much
  def myMax = vars.foldLeft(1)((acc,intvar) => acc * (if(math.abs(intvar.maxVal) > math.abs(intvar.minVal)) math.abs(intvar.maxVal) else math.abs(intvar.minVal)))
  def myMin = - myMax

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

  override def checkInternals(c:checker){
    var prod = 1;
    for (v <- vars) prod *= v.value
    c.check(output.value == prod)
  }
}

/** left - right
 * where left, right, and output are IntVar*/
case class Minus(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, ((l:Int, r:Int) => l-r), left.minVal - right.maxVal, left.maxVal - right.minVal){
  assert(left != right)
}

/** left + right
 * where left, right, and output are IntVar*/
case class Sum2(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, ((l:Int, r:Int) => l+r), left.minVal + right.minVal, left.maxVal + right.maxVal)

/** left * right
 * where left, right, and output are IntVar*/
case class Prod2(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, ((l:Int, r:Int) => l*r), Int.MinValue, Int.MaxValue)

/**left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual... */
case class Div(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, (l:Int, r:Int) => l/r)

/**left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual... */
case class Mod(left:IntVar, right:IntVar)
  extends IntVarIntVar2IntVarFun(left, right, (l:Int, r:Int) => l - r*(l/r))

/**abs(v) (absolute value)
 * where output and v are IntVar*/
case class Abs(v:IntVar)
  extends IntVar2IntVarFun(v, ((x:Int) => x.abs), (if(v.minVal<=0)0 else v.minVal), v.maxVal.max(-v.minVal))

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
