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

//naive NQueen,
// queens are always on different rows
//neightboorhood is queen swap
//selected neightbor is best pair to swap
//tabu on moved queens.
//jump away through random swap if plateau

package oscar.cbls.constraints.tests

import oscar.cbls.search._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.constraints.core._
import oscar.cbls.modeling.Algebra._
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.search.StopWatch

/**
 * Simple NQueen solved using a swap strategy with tabu search
 */
object NQueens extends SearchEngine with StopWatch{
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
    val tabulength = 0
    val m: Model = new Model(false,None,true)
    val MaxIT = 10000

    println("NQueens(" + N + ")")
    val Queens:Array[IntVar] = Array.tabulate(N)(q => IntVar(m, min, max, q, "queen" + q))

    val c:ConstraintSystem = new ConstraintSystem(m)

    //c.post(AllDiff(Queens)) handled trough permutations
    c.post(AllDiff(for ( q <- range) yield (q + Queens(q)).toIntVar))
    c.post(AllDiff(for ( q <- range) yield (q - Queens(q)).toIntVar))

    c.close()
//    m.close()
     m.close(false)
    println(m.dumpToDot(true,true))

    println("run time after model close: "+ getWatchString)

    var it:Int =0

    val Tabu = (for(q <- range)yield -1).toArray

    var longueurplateau = 0;
    while((c.Violation.value > 0) && (it < MaxIT)){
      val oldviolation:Int = c.Violation.value
      val allowedqueens = range.filter(q => Tabu(q) < it)
      val (q1,q2) = selectMin(allowedqueens,allowedqueens)((q1,q2) => c.swapVal(Queens(q1),Queens(q2)), (q1,q2) => q1 < q2)

      Queens(q1) :=: Queens(q2)
      Tabu(q1) = it + tabulength
      Tabu(q2) = it + tabulength

      it += 1
      println("it: " + it + " " + c.Violation + " (swapped "+ q1 + " and " + q2 + ")")
      if(oldviolation <= c.Violation.value) longueurplateau+=1 else longueurplateau = 0

      if (longueurplateau > 5){
        println("jump away")
        for (i <- 1 to N/5){
          Queens(selectFrom(range)) :=: Queens(selectFrom(range))
        }
        longueurplateau = 0
      }
    }
    println(c.Violation)
    println(m.getSolution(true))
    println("run time: "+ getWatchString)
  }
}
