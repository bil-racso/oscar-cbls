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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


/**
 * Testing of StockingCost constraint
 * 
 * @author: Vinasetan Ratheil HOUNDJI, ratheilesse@gmail.com
 *
 */

class TestGCCBC extends FunSuite with ShouldMatchers {

  def GccDecomp(cp: CPSolver, X: Array[CPIntVar], b: Map[Int, (Int, Int)]) = {
    
    //def gcc(x: Array[CPIntVar], values: Range, min: Array[Int], max: Array[Int]):
    val range = b.map(_._1).min to b.map(_._1).max
    val min = range.map(i => if (b.contains(i)) b(i)._1 else 0).toArray
    val max = range.map(i => if (b.contains(i)) b(i)._2 else X.size).toArray
    /*
    println("range:"+range)
    println("min:"+min.mkString(",")+" max:"+max.mkString(","))
    */
    cp.add(gcc(X,range,min,max),Strong)
    /*
    val max = X.map(_.max).max
    val min = X.map(_.min).min

    for ((t, (x, y)) <- b) {
      cp.add((sum(0 until X.size)(k => X(k) === t)) >= b(t)._1)
      cp.add((sum(0 until X.size)(k => X(k) === t)) <= b(t)._2)
    }*/

  }

  def nbSol(domX: Array[Set[Int]], b: Map[Int, (Int, Int)], decomp: Boolean): (Int, Int, Int) = {
    var nbSol = 0
    val cp = CPSolver()

    val X = Array.tabulate(domX.size)(i => CPIntVar(domX(i))(cp))

    if (decomp) {
      GccDecomp(cp, X, b)
    } else {
      cp.add(new GCCBC(X, b))
    }
    cp.search {
      binaryStatic(X)
    } onSolution {
      //             for (x <- X) print(x.mkString(","))
      //       println
      nbSol += 1
    }
    val stat = cp.start()
    (nbSol, stat.nFails, stat.nNodes)
  }

  test("GccBC1") {
    val x1 = (2 to 2).toSet
    val x2 = (1 to 2).toSet
    val x3 = (2 to 3).toSet
    val x4 = (2 to 3).toSet
    val x5 = (1 to 4).toSet
    val x6 = (3 to 4).toSet
    val domX = Array(x1, x2, x3, x4, x5, x6)

    val bounds = Map(1 -> (1, 3), 2 -> (1, 3), 3 -> (1, 3), 4 -> (2, 3))

    var (nSol1, nSol2) = (0, 0)
    var (bkt1, bkt2) = (0, 0)
    var (nNode1, nNode2) = (0, 0)

    val t1 = oscar.util.time {
      val (a, b, c) = nbSol(domX, bounds, true)
      nSol1 = a
      bkt1 = b
      nNode1 = c
    }
    val t2 = oscar.util.time {
      val (a, b, c) = nbSol(domX, bounds, false)
      nSol2 = a
      bkt2 = b
      nNode2 = c
    }

    nSol1 should equal(nSol2)


  }

  val rand = new scala.util.Random(0)
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)).toSet

  test("GccBC2") {
    var nbWins = 0
    for (i <- 1 to 100) {
      val nbVars = 8

      val domVars = Array.fill(nbVars)(randomDom(size = nbVars))

      val min = domVars.flatten.min
      val max = domVars.flatten.max

      var bounds: Map[Int, (Int, Int)] = Map()

      for (v <- min to max) {
        val b1 = rand.nextInt(nbVars - 7) +1
        val b2 = rand.nextInt(nbVars - 4) +1
        bounds += (v -> (b1.min(b2), b1.max(b2)))
      }

      var (nSol1, nSol2) = (0, 0)
      var (bkt1, bkt2) = (0, 0)
      var (nNode1, nNode2) = (0, 0)
      val t1 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, bounds, false)
        nSol1 = a
        bkt1 = b
        nNode1 = c
      }
      val t2 = oscar.util.time {
        val (a, b, c) = nbSol(domVars, bounds, true)
        nSol2 = a
        bkt2 = b
        nNode2 = c
      }
      nSol1 should equal(nSol2)

        nbWins += 1
        

    }
  }

}

        /*
        println("test num: " + i)
        for (x <- domVars) println(x.mkString(","))
        for (x <- bounds) println(x._1 + " " + x._2)
        println("nbWins: " + nbWins)
        println("time: " + t1 + " " + t2)
        println("nbBtk: " + bkt1 + " " + bkt2)
        println("nbNode: " + nNode1 + " " + nNode2)
        println("nbSol: " + nSol1 + " " + nSol2)
        println
        */