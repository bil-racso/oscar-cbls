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

package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints._
import oscar.cp.modeling._
import oscar.algo.DisjointSets
import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.variables.CPIntVar

/**
 * Based on algorithm desbcribed in Qimper and al. paper, "An efficient Bounds
 * Consistency Algorithm for the Global Cardinality Constraint"
 *
 * lbc and ubc (reverse lbc)
 *
 * @author: Vinasetan Ratheil HOUNDJI, ratheilesse@gmail.com
 */
class GCCBC(val Y: Array[CPIntVar], val bounds: Map[Int, (Int, Int)], val reverse: Boolean = true) extends Constraint(Y(0).store, "GCCBC") {
  val n = Y.size

  var domMaxMax = Int.MinValue
  var domMinMin = Int.MaxValue
  var k = 0
  while (k < n) {
    val m = Y(k).min
    val M = Y(k).max
    if (m < domMinMin) domMinMin = m
    if (M > domMaxMax) domMaxMax = M
    k += 1
  }

  //--------------sort incremental ------------
  val X = Array.tabulate(n)(Y(_))
  val sortX = Array.tabulate(X.size)(i => i)
  def sortIncremental() {
    var nn = X.size
    var i = 0
    do {
      var newn = 0
      i = 1
      while (i < nn) {
        if (Y(sortX(i - 1)).max > Y(sortX(i)).max) {
          val tmp = sortX(i - 1)
          sortX(i - 1) = sortX(i)
          sortX(i) = tmp
          newn = i
        }
        i += 1
      }
      nn = newn
    } while (nn > 0);
    k = 0;
    while (k < n) {
      X(k) = Y(sortX(k))
      k += 1
    }
  }
  //-------------------------------------------  

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (reverse) {

      val mX: Array[CPIntVar] = Y.map(-_)
      val mBounds: Map[Int, (Int, Int)] = bounds.map(t => (-t._1, t._2))

      if (s.post(new GCCBC(mX, mBounds, false)) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }

    Y.foreach(_.callPropagateWhenBoundsChange(this))
    propagate()
  }

  val newMin = Array.fill(n)(0)

  def inSets(x: CPIntVar, sets: ArrayBuffer[(Int, Int)]): Boolean = {
    //    for (s <- sets) {
    //      if (s._1 <= x.min && s._2 >= x.max) return true
    //    }
    k = 0
    while (k < sets.size) {
      if (sets(k)._1 <= x.min && sets(k)._2 >= x.max) return true
      k += 1
    }
    false
  }

  var stableSets = ArrayBuffer[(Int, Int)]()
  var bucketFailureMap:  scala.collection.mutable.Map[Int, (Int, Boolean)] =  scala.collection.mutable.Map()
  val sets = new DisjointSets(domMinMin, domMaxMax)

  override def propagate(): CPOutcome = {

   domMaxMax = Int.MinValue
   domMinMin = Int.MaxValue    
    k = 0
    while (k < Y.size) {
      val m = Y(k).min
      val M = Y(k).max
      if (m < domMinMin) domMinMin = m
      if (M > domMaxMax) domMaxMax = M
      k += 1
    }
    //    println("domBounds: " + domMinMin + " - " + domMaxMax)

    stableSets.clear()
    bucketFailureMap.clear()
    sets.reset()

    //        println("domBounds: " + domMinMin + " - " + domMaxMax)

    k = domMaxMax
    while (k >= domMinMin) {
      if (bounds.get(k) != None) {
        val failure = if (bounds(k)._1 > 0) true else false
        bucketFailureMap += (k -> (bounds(k)._1, failure))
      }
      k = k - 1
    }

    bucketFailureMap += ((domMinMin - 1) -> (Int.MaxValue, false))
    bucketFailureMap += ((domMaxMax + 1) -> (Int.MaxValue, false))

    //    k = domMinMin - 1
    //    while (k <= domMaxMax + 1) {
    //      println("k: " + k + " " + bucketFailureMap(k)._1 + " " + bucketFailureMap(k)._2)
    //      k = k + 1
    //    }

    sortIncremental()
    var i = 0
    while (i < n) {
      val a = X(i).min
      val b = X(i).max

      var z = a
      while (bucketFailureMap(z)._1 <= 0) {
        z = z + 1
      }

      if (z > a) {
        val minBZ = b.min(z)
        k = a
        while (k < minBZ) {
          sets.union(k, k + 1)
          k = k + 1
        }
      }
      if (z > b) {
        val S = sets.find(b)
        //        val stable = for (j <- S.min to S.max) yield j
        val stable = (S.min, S.max)
        stableSets += stable
      } else {
        val newnNbBucket = bucketFailureMap(z)._1 - 1
        bucketFailureMap(z) = (newnNbBucket, bucketFailureMap(z)._2)

        z = a
        while (bucketFailureMap(z)._1 <= 0) {
          z = z + 1
        }
        newMin(i) = a
        while (bucketFailureMap(newMin(i))._2 == false) {
          newMin(i) = newMin(i) + 1
        }
        //        println(newMin(i))

        if (z > b) {
          var j = b
          while (bucketFailureMap(j)._1 <= 0) {
            j = j - 1
          }
          j = j + 1
          while (j <= b) {
            bucketFailureMap(j) = (bucketFailureMap(j)._1, false)
            j = j + 1
          }
        }

      }
      i = i + 1
    }

    //        println("newMin: " + newMin.mkString(","))

    k = domMinMin
    while (k <= domMaxMax) {
      if (bucketFailureMap(k)._2 == true) { /* println("failure: "); */ return Failure }
      k = k + 1
    }
    //    println("X: " + X.mkString(","))
    i = 0
    while (i < n) {
      if (!inSets(X(i), stableSets)) {
        if (X(i).updateMin(newMin(i)) == Failure) return Failure
      }
      i = i + 1
    }
    Suspend
  }

}