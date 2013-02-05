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

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{Invariant, IntVar}

/**
 * This invariant maintains the predecessors of each node.
 *
 * Info :
 *  Convention:
      - value 0 to N-1 for routed node in preds array.
      - value N for unrouted node in preds array.
 * @param next the array of successors of each points (deposits and customers) of the VRP.
 * @param V the number of vehicles.
 */
case class Predecessor(next:Array[IntVar],V:Int) extends Invariant{

  val N = next.length
  registerStaticAndDynamicDependencyArrayIndex(next)
  finishInitialization();
  val preds = for(i<- 0 until N) yield if (i<V) new IntVar(model, 0, N, i, "preds" + i)
    else new IntVar(model, 0, N, N, "preds" + i)

  for(p <- preds) p.setDefiningInvariant(this)

  def length = N
  def apply(i:Int) = preds(i)

  override def notifyIntChanged(v:IntVar,index:Int,OldVal:Int,NewVal:Int){
    assert(next(index) == v)
    // it unroutes a node
    if(NewVal == N) preds(index) := N
    else preds(NewVal) := index
  }

  override def checkInternals(){
    for(n<- 0 until N){
      //n is unrouted
      if(next(n).value==N) assert(preds(n).value==N)
      // n is routed
      else  assert(n == preds(next(n).value).value)
      }
  }

  override def toString()={
    var toReturn = ""
    toReturn +="\npreds array: ["
    for (v <- preds){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn
  }
}

