/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Simple assignment problem in Oscar.

  From Wayne Winston "Operations Research",
  Assignment Problems, page 393f
  (This is a generalized version with an added test column)

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object Assignment extends CPModel {

  // some helper functions

  // scalarProduct(array, array)
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) : CPVarInt = 
    sum(Array.tabulate(t.length)(i=>t(i)*cost(i)))

  // scalarProduct(matrix, matrix)
  def scalarProduct(m: Array[Array[CPVarInt]], cost: Array[Array[Int]]) : CPVarInt = 
    sum(for{i <- 0 until m.length} yield scalarProduct(m(i), cost(i)))


  // Nicer syntax for a CPVarInt matrix 
  class CPVarIntMatrix(m: Array[Array[CPVarInt]]) {
    def row(r: Int) : Array[CPVarInt] =  m(r)
    def col(c: Int) : Array[CPVarInt] = for{r <- 0 until m.length} yield m(r)(c)
  }

  implicit def mat(t: Array[Array[CPVarInt]]) = new CPVarIntMatrix(t)

  def makeCPVarIntMatrix(cp: CPSolver, rows: Int, cols: Int, range: Range) =
    Array.fill(rows)(Array.fill(cols)((CPVarInt(cp, range))))


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // Problem instance
    // hakank: I added the fifth column to make it more
    //         interesting
    val rows = 4
    val cols = 5

    val ROWS = 0 until rows
    val COLS = 0 until cols

    val cost = Array(Array(14,  5, 8,  7,  15),
                     Array( 2, 12, 6,  5,   3),
                     Array( 7,  8, 3,  9,   7),
                     Array( 2,  4, 6, 10,   1))

    //
    // variables
    //
    val x = makeCPVarIntMatrix(cp, rows, cols, 0 to 1)
    val total_cost = scalarProduct(x, cost)

    //
    // constraints
    //
    var numSols = 0
    cp.minimize(total_cost) subjectTo {
      
      // Exacly one assignment per row (task),
      ROWS.foreach(i=> cp.add( sum(x.row(i) ) == 1))
      
      // At most one assignments per column (worker)
      COLS.foreach(j => cp.add( sum(x.col(j) ) <= 1))
      

    } exploration {
       
      cp.binaryMaxDegree(x.flatten)

      println("total_cost:" + total_cost)
      for(i <- ROWS) {
        println(x(i).mkString(""))
      }
      println()
      for(i <- ROWS) {
        println("Task" + i + " is done by " + 
                x(i).zipWithIndex.filter(_._1.getValue() == 1).map(_._2).mkString(""))
      }
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
