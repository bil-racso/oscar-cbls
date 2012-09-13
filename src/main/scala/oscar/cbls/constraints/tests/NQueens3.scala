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


package oscar.cbls.constraints.tests
import oscar.cbls.search._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.constraints.core._
import oscar.cbls.invariants.core.computation.Implicits._
import oscar.cbls.invariants.lib.numeric.Implicits._
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._

/**
 * Another variant of to solve the NQueen problem
 */
object NQueens3 extends SearchEngine with StopWatch{
  def main(args: Array[String]) {

    if (args.length==0) {
      println("argument: number_of_queens")
      sys.exit()
    }
    
    startWatch()

    val N:Int=args(0).toInt

    val min = 0
    val max = N-1
    val range:Range = Range(0,N)
    val tabulength = 4
    val MaxIT = 10000

    println("NQueens(" + N + ")")

    val m: Model = new Model(false,false,true)
    val it:Iterator[Int] = getRandomPermutation(N)
    val Queens:Array[IntVar] = (for (q <- range) yield new IntVar(m, min, max, it.next(), "queen" + q)).toArray

    val c:ConstraintSystem = new ConstraintSystem(m)
    
    //c.post(AllDiff(Queens)) //enforced because we swap queens and they are always alldiff
    c.post(AllDiff(for ( q <- range) yield (q plus Queens(q)).toIntVar))
    c.post(AllDiff(for ( q <- range) yield (q minus Queens(q)).toIntVar))

    for (q <- range){c.registerForViolation(Queens(q))}

    c.close()

    val ViolationArray:Array[IntVar] = (for(q <- range) yield c.getViolation(Queens(q))).toArray
    val Tabu:Array[IntVar] = (for (q <- range) yield new IntVar(m, 0, Int.MaxValue, 0, "Tabu_queen" + q)).toArray
    val It = new IntVar(m,0,Int.MaxValue,0,"it")
    val NonTabuQueens:IntSetVar = SelectLESetQueue(Tabu, It)
    val NonTabuMaxViolQueens:IntSetVar = ArgMaxArray(ViolationArray, NonTabuQueens)

    m.close()
   // m.close(false)
    //println(m.dumpToDot(true,true))

    println("run time after model close: "+ getWatchString)

    var longueurplateau = 0;
    var minviolationplateau = c.Violation.getValue()
    while((c.Violation.getValue() > 0) && (It.getValue() < MaxIT)){

      val q1 = selectFrom(NonTabuMaxViolQueens.value)
      val q2 = selectMin(NonTabuQueens.value)(q => c.getSwapVal(Queens(q1),Queens(q)), (q:Int) => q!=q1)

      Queens(q1) :=: Queens(q2)
      Tabu(q1) := It.getValue() + tabulength
      Tabu(q2) := It.getValue() + tabulength
      
      It.++
      
      println(It + " " + c.Violation + " (swapped "+ q1 + " and " + q2 + ")")

      if(minviolationplateau <= c.Violation.getValue()) longueurplateau+=1 else longueurplateau = 0
      minviolationplateau = minviolationplateau.min(c.Violation.getValue())
      if (longueurplateau > 5000){
        println("jump away")
        for (i <- 1 to N/5)Queens(selectFrom(range)) :=: Queens(selectFrom(range))
        longueurplateau = 0
        minviolationplateau = c.Violation.getValue()
      }
    }
    println(c.Violation)
    println(Queens.toList)
    println("run time: "+ getWatchString)
  }
}
