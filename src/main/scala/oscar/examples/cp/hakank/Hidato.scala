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
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._
import Array._


/*

  Hidato puzzle in Oscar.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object Hidato extends CPModel {

  def maxDomNotbound(vars: Iterable[CPVarInt]): Iterable[(CPVarInt, Int)] = {
    val notbound = vars.filterNot(_.isBound)
    if (notbound.nonEmpty) {
      val sizeMax = notbound.map(_.getSize).max
      notbound.zipWithIndex.filter {
        _._1.getSize == sizeMax
      }
    } else {
      Iterable()
    }
  }
 


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // solution:
    //  6 7 9
    //  5 2 8
    //  1 4 3
    // Missing: 3,4,5,7
    // val problem = List(List(6,0,9),
    //                    List(0,2,8),
    //                    List(1,0,0))

    // solution :
    // 
    // 45 44 41 40 39 30 31
    // 46 43 42 28 29 38 32
    // 47 1 3 26 27 33 37
    // 48 2 25 4 34 35 36
    // 49 16 24 23 5 6 8
    // 17 19 15 22 12 7 9
    // 18 20 21 14 13 11 10
    //   
    //   2.68s, 803 bkts
    // val problem = List(List( 0,44,41, 0, 0, 0, 0),
    //                    List( 0,43, 0,28,29, 0, 0),
    //                    List( 0, 1, 0, 0, 0,33, 0),
    //                    List( 0, 2,25, 4,34, 0,36),
    //                    List(49,16, 0,23, 0, 0, 0),
    //                    List( 0,19, 0, 0,12, 7, 0),
    //                    List( 0, 0, 0,14, 0, 0, 0))


    // Some problem from the book:
    // Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

    // Problem 15 (Intermediate)
    // 5.8s and 5676 backtracks with binary branch and binaryFirstFail
    val problem = List(List(64, 0, 0, 0, 0, 0, 0, 0),
                       List( 1,63, 0,59,15,57,53, 0),
                       List( 0, 4, 0,14, 0, 0, 0, 0),
                       List( 3, 0,11, 0,20,19, 0,50),
                       List( 0, 0, 0, 0,22, 0,48,40),
                       List( 9, 0, 0,32,23, 0, 0,41),
                       List(27, 0, 0, 0,36, 0,46, 0),
                       List(28,30, 0,35, 0, 0, 0, 0))


    // Problem 156 (Master)
    // (This seems to be harder to solve than the 12x12 prolem 188 below...)
    // val problem = List(List(88, 0, 0,100, 0, 0,37,0, 0,34),
    //                    List( 0,86, 0,96,41, 0, 0,36, 0, 0),
    //                    List( 0,93,95,83, 0, 0, 0,31,47, 0),
    //                    List( 0,91, 0, 0, 0, 0, 0,29, 0, 0),
    //                    List(11, 0, 0, 0, 0, 0, 0,45,51, 0),
    //                    List( 0, 9, 5, 3, 1, 0, 0, 0, 0, 0),
    //                    List( 0,13, 4, 0, 0, 0, 0, 0, 0, 0),
    //                    List(15, 0, 0,25, 0, 0,54,67, 0, 0),
    //                    List( 0,17, 0,23, 0,60,59, 0,69, 0),
    //                    List(19,0,21,62,63, 0, 0, 0, 0, 0))


     // Problem 188 (Genius)
     // val problem = List(List(  0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0),
     //                    List(136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0),
     //                    List(139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0),
     //                    List(  0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103),
     //                    List(  0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101),
     //                    List(  0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0),
     //                    List( 30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0),
     //                    List( 32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92),
     //                    List(  0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0),
     //                    List(  0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0),
     //                    List( 35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0),
     //                    List( 36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88))
  

    val n = problem.length
    val n2 = n*n

    //
    // variables
    //

    // the grid as an array
    val x = Array.fill(n*n)(CPVarInt(cp, 1 to n2))

    //
    // constraints
    //
    var numSols = 0
    var backtracks = 0
    cp.solveAll subjectTo {
      
      // fill the given hints
      for(i <- 0 until n) {
        for(j <- 0 until n) {
          if (problem(i)(j) > 0) {
            cp.add(x(i*n+j) == problem(i)(j))
          }
        }
      }

      cp.add(alldifferent(x), Strong)

      for(k <- 1 until n*n-1) {

        val i = CPVarInt(cp, 0 to n-1)
        val j = CPVarInt(cp, 0 to n-1)
        val a = CPVarInt(cp, -1 to 1) 
        val b = CPVarInt(cp, -1 to 1) 

        val ix1 = i*n+j
        val ix2 = (i+a)*n + j+b

        // keep inside the borders
        cp.add(i+a >= 0)
        cp.add(i+a <  n)
        cp.add(j+b >= 0)
        cp.add(j+b <  n)

        // must be a move
        cp.add(a.abs()+b.abs() > 0)

        // find the indices of this k
        // x(i)(j) #= k
        cp.add(element(x, ix1) == k, Strong)

        // and then find where to put the next number (k+1)
        // x(i+a)(j+b) #= k+1
        cp.add(element(x, ix2) == k+1, Strong)

      }
  


    } exploration {

 
        // cp.binaryFirstFail(x)
        cp.binary(x)
        // cp.binaryMaxDegree(x) // this is slower


        while (false && !allBounds(x)) {

          // all unbound variables
          val notbound = x.filterNot(_.isBound)

          // This is a test of many variant of variable/value selections

          //
          // variable selection
          //

          // Here are some of the standard heuristics:
          // 
          // input order: selects the first variable from the current input
          //              vector,
          // first fail: selects a variable with the smallest number of
          //             elements in its domain,
          // most constrained: selects a variable that appears in most
          //                   constraints,
          // smallest: selects a variable that has the smallest minimal
          //           value in its domain,
          // largest: selects a variable that has the greatest minimal
          //          value in its domain,
          // smallest maximal: selects a variable that has the smallest
          //                   maximal value in its domain, and
          // max regret: selects a variable that has the largest difference
          //             between the smallest and the second smallest value in its domain.


          val (y,i) = minDomNotbound(x).head  // first of those vars with smallest domains
          // val (y,i) = minDomNotbound(x).last // last of those vars with smallest domains


          // val (y,i) = maxDomNotbound(x).head // defined above (for the vars with largest domains)
          // val (y,i) = maxDomNotbound(x).last // defined above: not bad
          // val y = notbound.head
          // val y = notbound.last

          // "first fail": selects a variable with the smallest number of
          //               elements in its domain,
          // val y = argMin(notbound)(v=>v.getSize()).head
          // val y = argMin(notbound)(v=>v.getSize()).last

          // val y = argMax(notbound)(v=>v.getSize()).head
          // val y = argMax(notbound)(v=>v.getSize()).last

          // "Smallest"
          // val y = argMin(notbound)(v=>v.getMin()).head
          // val y = argMin(notbound)(v=>v.getMin()).last
          // val y = argMax(notbound)(v=>v.getMin()).head
          // val y = argMax(notbound)(v=>v.getMin()).last

          // "Largest"
          // val y = argMin(notbound)(v=>v.getMax()).head
          // val y = argMax(notbound)(v=>v.getMax()).head

          // "max regret"
          // val y = argMin(notbound)(v=>v.getMax()-v.getMin()).head
          // val y = argMin(notbound)(v=>v.getMax()-v.getMin()).last
          // val y = argMax(notbound)(v=>v.getMax()-v.getMin()).head
          // val y = argMax(notbound)(v=>v.getMax()-v.getMin()).last

          // "maxConstraintDegree"
          // val y = argMin(notbound)(v=>v.getConstraintDegree()).head
          // val y = argMin(notbound)(v=>v.getConstraintDegree()).last
          // val y = argMax(notbound)(v=>v.getConstraintDegree()).head
          // val y = argMax(notbound)(v=>v.getConstraintDegree()).last

          //    
          // value selection
          // 
          val size = y.getSize
          val vMin = y.getMin()   // min value of domain
          val vMax = y.getMax()   // max value of domain
          val vMidV = ((vMin + vMax) / 2).toInt; // calculate median value (of vMin and vMax)
          val vMid = y.getValueAfter(vMidV)  // the median value in the domain
          val vRand = y.getRandomValue()  // random value from domain

          // println("y: " + y)
          // println("vMin: " + vMin + " (vMidV: " + vMidV + ") vMid: " + vMid + " vMax: " + vMax)

          var v = vRand

    	  cp.branch {
            cp.post(y == v)
          } {
            cp.post(y != v)
          }
         
      } // while(!allBounds(x)


      println("\nSolution:")
      for(i <- 0 until n) {
        for(j <- 0 until n) {
          print("%4d".format(x(i*n+j).getValue()))
        }
        println()
      }
      println()

      numSols += 1

   }

    println("It was " + numSols + " solutions.")

    cp.printStats()

  }

}
