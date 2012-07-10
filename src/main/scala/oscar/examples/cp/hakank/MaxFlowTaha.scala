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
 
   Max flow problem in Oscar

   From Taha "Introduction to Operations Research", Example 6.4-2

   Translated from the AMPL code at
   http://taha.ineg.uark.edu/maxflo.txt

   
   @author Hakan Kjellerstrand hakank@gmail.com
   http://www.hakank.org/oscar/
 
 */
object MaxFlowTaha extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n     = 5;
    val start = 0;
    val end   = n-1;

    val NODES = 0 until n

    // cost matrix
    val c = Array(Array(0, 20, 30, 10,  0),
                  Array(0,  0, 40,  0, 30),
                  Array(0,  0,  0, 10, 20),
                  Array(0,  0,  5,  0, 20),
                  Array(0,  0,  0,  0,  0))

      
    // variables
    val x = Array.tabulate(n)(i=>
                              Array.tabulate(n)(j =>
                                                CPVarInt(cp, 0 to c(i)(j))))

    val out_flow = Array.fill(n)(CPVarInt(cp, 0 to 1000))
    val in_flow = Array.fill(n)(CPVarInt(cp, 0 to 1000))
    val total = CPVarInt(cp, 0 to 10000)


    //
    // constraints
    //
    var numSols = 0

    cp.maximize(total) subjectTo {

      cp.add( sum(for{j <- NODES
                      if c(start)(j) > 0} yield x(start)(j)) == total)
      
      for(i <- NODES) {
        val in_flow_sum = for{j <- NODES if c(j)(i) > 0} yield x(j)(i)
        if (in_flow_sum.length > 0) {
          cp.add(sum(in_flow_sum)  == in_flow(i))
        }

        val out_flow_sum = for(j <- NODES if c(i)(j) > 0) yield x(i)(j)

        if (out_flow_sum.length > 0) {
          cp.add(sum(out_flow_sum)  == out_flow(i))
        }
        
      }
      
      // in_flow == out_flow
      for(i <- NODES if i != start && i != end) {
        cp.add(out_flow(i) == in_flow(i))
      }
      
      val s1 = for(i <- NODES if c(i)(start) > 0) yield x(i)(start)
      if (s1.length > 0) {
        cp.add(sum(s1) == 0)
      }
      
      val s2 = for(j <- NODES if c(end)(j) > 0) yield x(end)(j)
      if (s2.length > 0) {
        cp.add(sum(s2) == 0)
      }


     } exploration {
       
      cp.binary(x.flatten)
      // cp.binaryFirstFail(x.flatten)
      // cp.binaryMaxDegree(x.flatten)

      println("total: " + total)
      for(i <- NODES) {
        for(j <- NODES) {
          print("%3d".format(x(i)(j).getValue()))
        }
        println()
      }
      println()

      numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
