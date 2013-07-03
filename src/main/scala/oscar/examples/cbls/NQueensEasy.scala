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

package oscar.examples.cbls

import oscar.cbls.modeling.Algebra._
import oscar.cbls.constraints.core._
import oscar.cbls.modeling._
import oscar.util._

/**
 * Local Search for NQueens
 * Moves are operated by swapping variables
 * 
 */
object NQueensEasy extends App{

    val N = 20
  
    println("N-Queens(" + N + ")")

    val rand = new scala.util.Random()
    
    val ls = new LSSolver()
    val init = rand.shuffle((0 to N-1).toList).toArray // initial solution
    val queens = Array.tabulate(N)(q => new LSVarInt(ls, 0, N-1,init(q),"queen" + q))
    val c = new ConstraintSystem(ls)

    //alldiff on rows in enforced because we swap queens initially different
    c.post(allDifferent(Array.tabulate(N)(q => (queens(q) + q).toIntVar))) // I would prefer not have to call toIntVar
    c.post(allDifferent(Array.tabulate(N)(q => (q - queens(q)).toIntVar)))

    val violations = Array.tabulate(N)(q => c.violation(queens(q)))
    val maxViolQueens = argMax(violations) // set of queens with highest violation

    c.close()
    ls.close()

  //this tabu search is a bit simplistic: does not use the invariants for maintining Tabu...
  //and max violated queens might be all tabu

    val pairs = (0 until N) x  (0 until N)
    
    var it = 0
    val tabu = Array.fill(N,N)(0)
    val tenure = 3
    def isNotTabu(pair: (Int,Int)) = pair._1 < pair._2 && tabu(pair._1)(pair._2) <= it 
    
    while(c.violation.value > 0){
      val (q1,q2) = selectMin(pairs)(q => isNotTabu(q))(q => c.swapVal(queens(q._1),queens(q._2))).get      
      queens(q1) :=: queens(q2)
      tabu(q1)(q2) = it + tenure
      it += 1
    }
    println("number of iterations:"+it)
    println(queens.mkString(","))

  
}
