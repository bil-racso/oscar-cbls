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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.invariants.tests
import oscar.cbls.search._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.lib.set._
import oscar.cbls.invariants.lib.minmax._
import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation.Implicits._
import oscar.cbls.algebra.Algebra._


object test extends SearchEngine {

  def main(args: Array[String]) {

    val m: Model = new Model

    val min = 0
    val max = 100

    val a:IntVar = new IntVar(m, min, max, 9, "a")
    val b:IntVar = new IntVar(m, min, max, 5, "b")
    val c:IntVar = new IntVar(m, min, max, 6, "c")
    val d:IntVar = new IntVar(m, min, max, 6, "d")
    val e:IntSetVar = new IntSetVar(m, min, max, "e", SortedSet.empty[Int])

    d <== (5 + c + 5 + (b * (4 - 3)))
    c <== a + b //Sum(SortedSet(a, b))
    e <== Inter(MakeSet(SortedSet(a, b)), MakeSet(SortedSet(b, c)))
    val Const5 = IntConst(5,m)
    val f:IntVar = Max(SortedSet(a,b,c,d)) + MinLin(SortedSet(Abs(a),b,Const5,d))
    val g:IntVar = MaxLin(SortedSet(a,b,c,d))

    val h:IntSetVar = ArgMinArray(Array(a,d,b,d,c,d - 1))
  
    Event(h,{println("Trigger: h changed: " + h)})
    val k:IntVar = Cardinality(h)
    Event(k,{println("Trigger: k changed: " + k)})

    Event(c,{println("Trigger: c changed: " + c)})

    //TriggerOn(cstr.getViolation(d), {println("Violation change on d: " + e)})

    m.close()
    println("closed")

    for(j <- Range(1,100,1)){a := j; println(h)}
    a := 5
    println("just changed a to 5")
    println("" + c + " " + d)
    b := 2
    println("just changed b to 2")
    println("" + c + " " + d)
    m.propagate()
    
    //while(cstr.Violation != 0){
    //  val tochange:IntVar = selectMax(List(a,b),v => cstr.getViolation(v))
    //  val newval = selectMin(tochange.getDomain(), i => cstr.GetAssignDelta(tochange,i))
    //  tochange := newval
    //}

    println("" + c + " " + d)

    println(m.getSolution(false))

  }
}
