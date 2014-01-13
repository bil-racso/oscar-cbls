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

import oscar.cbls.search._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.constraints.core._
import oscar.cbls.modeling.Algebra._
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import util.Random
import oscar.cbls.invariants.core.computation.IntVar.int2IntVar
import oscar.cbls.invariants.lib.logic.SelectLESetQueue
;

//Beware: this requires a lot of memory, so I use to put this in the command line.
//-Xms1000M -Xmx1000M

/** 
 * NQueen for larger problems : 
 * - queens are always on different rows 
 * - neightboorhood is queen swap
 * - first queeen is among most violated ones; maintained through invariant
 * - second is first one leading to a decrease in violation 
 * when swap is performed tabu on moved queens.
 * - jump away through random swap if plateau
 * 
 * The program accepts an argument which is the problem size
 * Otherwise it performs a benchmarking over a range of sizes (this takes time)
*/
object NQueensBench1 extends SearchEngine(true) with StopWatch{

  def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
  def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

  def main(args: Array[String]) {

    if (args.length<1) {
      println("Benchmarking NQueen - this takes time")
      println("advise: put -Xms1000M -Xmx1000M")
      println(padToLength("N", 15) + padToLength("tClose[ms]", 15) + padToLength("tTotal[ms]", 15) + "it")
    
      // first run could have some overhead so ignoring it
      SolveNQueen(1000)

      // multiple runs
      for (n <- Range(1000, 11000, 1000)){
    	  SolveNQueen(n)
    	  System.gc()
      }
    } else {
    	val N:Int=args(0).toInt
      	SolveNQueen(N)
    }
  }
  
  def SolveNQueen(N:Int){
    print(padToLength("" + N, 15))

    startWatch()
    val range:Range = Range(0,N)
    val tabulength = 10

    val m: Store = new Store(false,None,true)
    val it = Random.shuffle(range.toList).iterator
    val Queens:Array[IntVar] = (for (q <- range) yield IntVar(m, 0, N-1,it.next(), "queen" + q)).toArray

    val c:ConstraintSystem = new ConstraintSystem(m)

    //c.post(AllDiff(Queens)) //enforced because we swap queens and they are always alldiff
    c.post(AllDiff(for ( q <- range) yield (Queens(q) + q).toIntVar))
    c.post(AllDiff(for ( q <- range) yield (q - Queens(q)).toIntVar))

    val ViolationArray:Array[IntVar] = (for(q <- range) yield c.violation(Queens(q))).toArray

    val Tabu:Array[IntVar] = (for (q <- range) yield IntVar(m, 0, Int.MaxValue, 0, "Tabu_queen" + q)).toArray
    val It = IntVar(m,0,Int.MaxValue,1,"it")
    val NonTabuQueens:SetVar = SelectLESetQueue(Tabu, It)
    val NonTabuMaxViolQueens:SetVar = new ArgMaxArray(ViolationArray, NonTabuQueens)

    m.close(false)
    print(padToLength("" + getWatch, 15))

    while((c.Violation.value > 0) && (It.value < N)){
      val oldviolation:Int = c.Violation.value

      // to ensure that the set of tabu queens is no too restrictive
      // (but you'd better tune the tabu better)
      while(NonTabuMaxViolQueens.value.isEmpty){
        It ++;
        println("Warning: Tabu it too big compared to queens count")
      }

      val q1 = selectFirst(NonTabuMaxViolQueens.value)
      val q2 = selectFirst(NonTabuQueens.value, (q:Int) => {
        q!=q1 && c.swapVal(Queens(q1),Queens(q)) < oldviolation
      })

      Queens(q1) :=: Queens(q2)
      Tabu(q1) := It.getValue(true) + tabulength
      Tabu(q2) := It.getValue(true) + tabulength
      
      It ++
    }

    println(padToLength("" + getWatch, 15) + It.value)
  }
}
