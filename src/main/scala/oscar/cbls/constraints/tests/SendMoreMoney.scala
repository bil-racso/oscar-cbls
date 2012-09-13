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
 *         by Renaud De Landtsheer and Christophe Ponsard
 ******************************************************************************/


package oscar.cbls.constraints.tests

import oscar.cbls.search._
import oscar.cbls.constraints.core._
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.computation.Implicits._
import oscar.cbls.invariants.lib.numeric.Implicits._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.algebra.Algebra._

/**
 * Very simple example showing how to use Asteroid on the basic SEND+MORE=MONEY
 * Using a generic constrained directed search
 * 
 */
object SendMoreMoney extends SearchEngine with StopWatch {
  
  def main(args: Array[String]) {
    
    startWatch()
    
    println("SEND+MORE=MONEY")

    // enum letters
    object Letter extends Enumeration {
      type Letter = Value
      val S,E,N,D,M,O,R,Y,X1,X2 = Value
      val list=List[Letter](S,E,N,D,M,O,R,Y,X1,X2)
    }    
    
    print("Letter: ")
    for (l <- Letter.list) { print( l +" ") }
    println()
        
    // 4 carries
    object Carry extends Enumeration {
      type Carry = Value
      val c1,c2,c3,c4 = Value
      val list=List[Carry](c1,c2,c3,c4)
    }    
    print("Carries: ")
    for (c <- Carry.list) { print( c + " ") }
    println()
    
    // Search control
    val MAX_IT = 10000
    val TABU_LENGTH = 4

    // model
    val m: Model = new Model(false,false,true)
        
    // letter and carriage values
    // d initialised with 0..10, r with 0
    val d:Array[IntVar]= (for(l <- Letter.list) yield new IntVar(m, 0, 9, l.id, l+"")).toArray
    val r:Array[IntVar]= (for(c <- Carry.values) yield new IntVar(m, 0, 9, 0, c+"")).toArray
          
    // constraint system
    val c:ConstraintSystem = new ConstraintSystem(m)
    
    // all digits should be different
    // c.post(AllDiff(d)) // will be enforced
    c.post(NE(d(Letter.S.id),0))
    c.post(NE(d(Letter.M.id),0))
    c.post(EQ(r(Carry.c4.id),d(Letter.M.id)))
    c.post((r(Carry.c3.id) + d(Letter.S.id) + d(Letter.M.id)) === (d(Letter.O.id) + (10 * d(Letter.M.id))))
    c.post((r(Carry.c2.id) + d(Letter.E.id) + d(Letter.O.id)) === (d(Letter.N.id) + (10 * r(Carry.c3.id))))
    c.post((r(Carry.c1.id) + d(Letter.N.id) + d(Letter.R.id)) === (d(Letter.E.id) + (10 * r(Carry.c2.id))))
    c.post(                    (d(Letter.D.id) + d(Letter.E.id)) === (d(Letter.Y.id) + (10 * r(Carry.c1.id))))

    r(Carry.c1.id) <== ((d(Letter.D.id) + d(Letter.E.id)) / 10)
    r(Carry.c2.id) <== ((d(Letter.N.id) + d(Letter.R.id) + r(Carry.c1.id)) / 10)
    r(Carry.c3.id) <== ((d(Letter.E.id) + d(Letter.O.id) + r(Carry.c2.id)) / 10)
    r(Carry.c4.id) <== ((d(Letter.S.id) + d(Letter.M.id) + r(Carry.c3.id)) / 10)
    
    for(l <- Letter.list) {c.registerForViolation(d(l.id))}
//    for(i <- Carry.list)  {c.registerForViolation(r(i.id))} // carries will be enforced so no violation
    c.close()

    // search variables
    val ViolationArray:Array[IntVar] = (for(l <- Letter.list) yield c.getViolation(d(l.id))).toArray
    val Tabu:Array[IntVar] = (for (i <- Letter.list) yield new IntVar(m, 0, Int.MaxValue, 0, "Tabu_" + i)).toArray
    val It = new IntVar(m,0,Int.MaxValue,0,"it")
    val NonTabuLetter:IntSetVar = SelectLESetQueue(Tabu, It)
    val NonTabuMaxViolLetter:IntSetVar = ArgMaxArray(ViolationArray, NonTabuLetter)
    
    // closing model
    m.close()

    // search
    while((c.Violation.getValue() > 0) && (It.getValue() < MAX_IT)){
      val l1 = selectFrom(NonTabuMaxViolLetter.value)
      val l2 = selectMin(NonTabuLetter.value)(i => c.getSwapVal(d(l1),d(i)), (i:Int) => i!=l1)

      // swapping so this enforces all d are different
      d(l1) :=: d(l2)
      println(c.Violation.toString +" "+c.Violation.getValue()+" "+c.getSwapVal(d(l1),d(l2)))
      // enforcing carries are matching constraints

      //r(Carry.c1.id).setValue((d(Letter.D.id).getValue()+d(Letter.E.id).getValue()) / 10)
      //r(Carry.c2.id).setValue((d(Letter.N.id).getValue()+d(Letter.R.id).getValue()+r(Carry.c1.id).getValue()) / 10)
      //r(Carry.c3.id).setValue((d(Letter.E.id).getValue()+d(Letter.O.id).getValue()+r(Carry.c2.id).getValue()) / 10)
      //r(Carry.c4.id).setValue((d(Letter.S.id).getValue()+d(Letter.M.id).getValue()+r(Carry.c3.id).getValue()) / 10)

      Tabu(l1) := It.getValue() + TABU_LENGTH
      // Tabu(l2) := It.getValue() + TABU_LENGTH // not a good idea to tabu the second variable
      
      It.++ // try without the dot (strange line behavior)
      println(It.toString + " " + c.Violation+" switching "+d(l1).name+" "+d(l2).name)
    }	
      
    println("Solution: "+m.getSolution(true))
    println("run time: "+ getWatchString)
  }

}
