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

package oscar.examples.cbls.queens

import oscar.cbls.modeling.CBLSModel
import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar}
import scala.language.postfixOps

/**
 * Created by rdl on 13/01/14.
 */
object NQueensEasy2 extends CBLSModel with App{

  val N = 20

  println("NQueens(" + N + ")")

  val rand = new scala.util.Random()

  startWatch()

  // initial solution
  val init = rand.shuffle((0 to N-1).toList).toArray

  val queens = Array.tabulate(N)(q => CBLSIntVar(0 to N-1,init(q),"queen_" + q))

  //alldiff on rows in enforced because we swap queens initially different
  add(allDifferent(Array.tabulate(N)(q => (queens(q) + q).toIntVar)))
  add(allDifferent(Array.tabulate(N)(q => (q - queens(q)).toIntVar)))

  val violationArray:Array[CBLSIntVar] = Array.tabulate(N)(q => c.violation(queens(q))).toArray

  val tabu:Array[CBLSIntVar] = Array.tabulate(N)(q => CBLSIntVar(0 to Int.MaxValue, 0, "tabu_queen" + q))
  val it:CBLSIntVar = CBLSIntVar(0 to Int.MaxValue,1,"it")
  val nonTabuQueens:CBLSSetVar = selectLESetQueue(tabu, it)
  val nonTabuMaxViolQueens:CBLSSetVar = argMax(violationArray, nonTabuQueens)

  close()

  val tabulength = 3

  while((violation.value > 0) && (it.value < N)){

    val oldviolation:Int = violation.value

    selectFirstDo(nonTabuMaxViolQueens.value)((q1:Int) => {
      selectFirstDo(nonTabuQueens.value, (q2:Int) => {
        q2!=q1 && c.swapVal(queens(q1),queens(q2)) < oldviolation
      })((q2:Int) => {
        println("" + it.value + " swapping " + q1 +"(tabu: " + tabu(q1) + ") and " + q2 +"(tabu: " + tabu(q2) + ")")
        queens(q1) :=: queens(q2)
        tabu(q1) := it.value + tabulength
        tabu(q2) := it.value + tabulength

      },()=>{})},
      ()=>{})

    it ++

  }

  println(getWatchString)
  println(it)
  println(queens.mkString(","))
}
