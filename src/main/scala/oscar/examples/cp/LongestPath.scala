/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._

/**
 * Longest Path Problem
 * @author Pierre Schaus pschaus@gmail.com
 */
object LongestPath extends App {

  // --- reading the data ---

  val lines = Source.fromFile("data/longestpath/planar-n50.ins1.txt_com10_ins1").getLines.reduceLeft(_ + " " + _)
  //val lines = Source.fromFile("data/longestpath/test.txt").getLines.reduceLeft(_ + " " + _)
  
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "")
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }
  val nbNodes = next().toInt // last one is a dummy start/end to use circuit
  val nbArcs = next().toInt
  val nodes = 0 until nbNodes
  println("nbNodes:" + nbNodes + " nbArcs:" + nbArcs)
  val absent = -10000000
  val distMatrix = Array.fill(nbNodes, nbNodes)(absent)
  for (i <- 1 to nbArcs) {
    val from = next().toInt - 1
    val to = next().toInt - 1
    val w: Int = (next().toDouble * 100).toInt
    distMatrix(from)(to) = w
    distMatrix(to)(from) = w
  }

  def successors(i: Int): Set[Int] = nodes.filter(distMatrix(i)(_) != absent).toSet

  val outNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)
  val inNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)

  // set of valid transitions pair
  val tuples = (for (i <- nodes; j <- successors(i)) yield (i, j)).toSet

  val distMatrix_ = Array.tabulate(nbNodes, nbNodes)((i, j) => if (distMatrix(i)(j) == absent) 0 else distMatrix(i)(j))

  val len = 12 // path lenth

  println("----------- trying with length:" + len + "-------------")
  val cp = CPSolver()
  val path = Array.fill(len)(CPVarInt(cp, nodes))
  val weight = sum(0 until len - 1)(i => distMatrix_(path(i))(path(i + 1)))
  //println((for (i <- nodes) yield distMatrix_(i).mkString(",")).mkString("\n"))

  
  
  
  cp.maximize(weight) subjectTo {
    for (i <- 0 until len - 1) {
      cp.add(table(path(i), path(i + 1), tuples)) // for each consecutive visits, give the possible valid transitions
    }
    cp.add(allDifferent(path), Weak)
    //val cons = new WeightedPath(path, outNodes, inNodes, distMatrix_, weight)
    //println("initlb:"+cons.bound(true)+" initub:"+cons.bound(false))
    //cp.post(cons)

  } exploration {
    cp.binaryFirstFail(path)

    //println(path.mkString(","))
  }
  cp.printStats

}

class WeightedPath(val path: Array[CPVarInt], outNodes: Array[Set[Int]], inNodes: Array[Set[Int]], distMatrix: Array[Array[Int]], dist: CPVarInt) extends Constraint(path(0).s) {

  val n = outNodes.length
  val m = path.length

  def isConsistent(): Boolean = check(m - 2)

  def check(l: Int): Boolean = {
    if (l >= 0) {
      val ok = (0 until n).forall(i => outNodes(i).filter(path(l + 1).hasValue(_)).nonEmpty)
      for(i <- 0 until n) {
        val empty = outNodes(i).filter(path(l + 1).hasValue(_)).isEmpty
        if (empty) {
          if (path(l).removeValue(i) == CPOutcome.Failure) {
            return false
          }
        }
      }
      true
    } else {
      true
    }
  }

  def bound(lower: Boolean): Int = {
    val next = Array.fill(m, n)(0)
    val weight = Array.fill(m, n)(if (lower) 1000000 else -1000000)
    for (i <- 0 until n) {
      if (path(m-1).hasValue(i)) {
        weight(m-1)(i) = 0
      } else {
        if (lower) weight(m-1)(i) = 1000000
        else weight(m-1)(i) = -1000000
      }
       
    }
    def updateLayer(l: Int) {
      if (l >= 0) {
        for (i <- 0 until n; if (path(l).hasValue(i)) ) {
          val validOutNodes = outNodes(i).filter(path(l + 1).hasValue(_))
          val pairs = validOutNodes.map(j => (distMatrix(i)(j) + weight(l + 1)(j), j))
          val (cost, succ) = if (lower) pairs.min else pairs.max
          next(l)(i) = succ
          weight(l)(i) = cost
        }
        updateLayer(l - 1)
      }
    }
    updateLayer(m - 2)
    
    val weight0 = (0 until n).filter(path(0).hasValue(_)).map(weight(0)(_))
    if (lower) weight0.min else weight0.max
    
  }

  def setup(strength: CPPropagStrength): CPOutcome = {
    path.foreach(_.callPropagateWhenDomainChanges(this))
    dist.callPropagateWhenBoundsChange(this)
    propagate()
  }

  override def propagate(): CPOutcome = {
    if (isConsistent()) {
      
      //println("lb:"+bound(true)+" cost:"+dist)
      
      if (dist.updateMin(bound(true)) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
      
      if (dist.updateMax(bound(false)) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
      return CPOutcome.Suspend
    } else CPOutcome.Failure
  }

}
