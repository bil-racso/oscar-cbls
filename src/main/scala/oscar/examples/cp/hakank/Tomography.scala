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


/**

  Discrete Tomography problem in Oscar.

  Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.
  
  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:
  
  ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0                         
  0                         
  8      * * * * * * * *    
  2      *             *    
  6      *   * * * *   *    
  4      *   *     *   *    
  5      *   *   * *   *    
  3      *   *         *    
  7      *   * * * * * *    
  0                         
  0                         


  Eclipse solution by Joachim Schimpf, IC-Parc
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object Tomography extends CPModel {

 
  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    // 
    // These following three examples are from the ECLiPSe program cited above.
    // 
    // val row_sums = List(0,0,8,2,6,4,5,3,7,0,0)
    // val col_sums = List(0,0,7,1,6,3,4,5,2,7,0,0)
 
    // val row_sums = List(10,4,8,5,6)
    // val col_sums = List(5,3,4,0,5,0,5,2,2,0,1,5,1)


    // This give three slightly different solutions.
    // val row_sums = List(11,5,4)
    // val col_sums = List(3,2,3,1,1,1,1,2,3,2,1)
                 

    // This is my (hakank's) own problem.
    val row_sums = List(0,2,2,2,2,2,8,8,4,4,4,4,4,0)
    val col_sums = List(0,0,0,12,12,2,2,2,2,7,7,0,0,0)


    val r = row_sums.length
    val c = col_sums.length


    // variables
    val x = Array.fill(r)(Array.fill(c)(CPVarInt(cp, 0 to 1)))


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      // rows
      for(i <- 0 until r) {
        cp.add(sum( Array.tabulate(c)(j=> x(i)(j)) ) == row_sums(i))
      }
      
      // columns
      for(j <- 0 until c) {
        cp.add(sum( Array.tabulate(r)(i=> x(i)(j)) ) == col_sums(j))
      }


     } exploration {
       
       cp.binaryFirstFail(x.flatten)

       println("\nSolution:")
       print("     ")
       for(j <- 0 until c) {
         print("%2d".format(col_sums(j)) )
       }
       println()
       for(i <- 0 until r) {
         print(" " + "%2d".format(row_sums(i)) + "   ")
           for(j <- 0 until c) {
             if (x(i)(j).getValue() == 1) {
               print("X ") 
             } else {
               print("  ")
             }
           }
           println()
       }
       println()


       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
