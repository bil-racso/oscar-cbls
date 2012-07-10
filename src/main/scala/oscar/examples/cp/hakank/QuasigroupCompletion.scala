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

 Quasigroup Completion problem in Oscar.

 See 
 Carla P. Gomes and David Shmoys:
 "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
  
 See also
 Ivars Peterson "Completing Latin Squares"
 http://www.maa.org/mathland/mathtrek_5_8_00.html
 """
 Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
 a four-by-four array so that no column or row contains the same two numbers. 
 The result is known as a Latin square.
 ...
 The so-called quasigroup completion problem concerns a table that is correctly 
 but only partially filled in. The question is whether the remaining blanks in 
 the table can be filled in to obtain a complete Latin square (or a proper 
 quasigroup multiplication table).
 """
 
 @author Hakan Kjellerstrand hakank@gmail.com
 http://www.hakank.org/oscar/
 
*/
object QuasigroupCompletion extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    val X = 0

    // Default problem instance 
    // Example from Gomes & Shmoys, page 7
    // Two solutions.
   var n = 4 
   var problem = List(List(X,1,X,X),
                      List(X,X,2,X),
                      List(X,3,X,X),
                      List(X,X,X,4))

  // read problem instance from file
  if (args.length > 0) {
    println("\nReading from file: " + args(0))
    
    val lines = fromFile(args(0)).getLines.filter(!_.startsWith("#")).toList
    n = lines(0).toInt
    println("n:" + n)
    var this_problem = List[List[Int]]()
    for (t <- 1 to n) {
      println(lines(t))
      val t2: List[Int] = lines(t).split(" ").toList.map(i=>if (i == ".") X else i.toInt)
      this_problem = this_problem ::: List(t2)
    }
    println()

    problem = this_problem
  }
  


    // variables
    val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 1 to n)))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {


      // fill the things we know
      for (i <- 0 until n; j <- 0 until n) {
        if (problem(i)(j) > X) {
          cp.add(x(i)(j) == problem(i)(j))
        }
      }
  
      // make it a Latin Square
      for (i <- 0 until n) {
        cp.add(alldifferent( Array.tabulate(n)(j=> x(i)(j)) ), Strong)
        cp.add(alldifferent( Array.tabulate(n)(j=> x(j)(i)) ), Strong)
      }


     } exploration {
       
       cp.binaryFirstFail(x.flatten)

       println("\nSolution:")
       for(i <- 0 until n) {
         for(j <- 0 until n) {
           print(x(i)(j) + " ")
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
