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

package oscar.util.mo

import math.Numeric.Implicits._
//import Implicits._
import scala.Array.canBuildFrom
import scala.Option.option2Iterable

/**
 * QuadTree able to maintain a set of non dominated vectors (assuming maximizations in all dimensions).
 * @author Pierre Schaus pschaus@gmail.com
 */
class QuadTree[T <% Ordered[T]](private var vector: Array[T]) {
  


  val k = vector.size
  private val twoPowK: Int = math.pow(2, k).toInt
  private var sons: Array[Option[QuadTree[T]]] = Array.fill(twoPowK)(None)
  private def sonsFrom(i: Int) = i to (twoPowK - 2)

  def apply(i: Int): T = vector(i)

  private def inS0(theta: Int, i: Int): Boolean = theta >> i == 0
  private def inS1(theta: Int, i: Int): Boolean = theta >> i == 1
  private def inS0(S0: Iterable[Int], theta: Int): Boolean = S0.forall(inS0(theta, _))
  private def inS1(S1: Iterable[Int], theta: Int): Boolean = S1.forall(inS1(theta, _))
  private def S0phi(zs: QuadTree[T]) = for (i <- 0 until k; if vector(i) >= zs.vector(i)) yield i
  private def S1phi(zs: QuadTree[T]) = for (i <- 0 until k; if vector(i) < zs.vector(i)) yield i
  private def phi(zs: QuadTree[T]) = for (i <- 0 until k) yield if (vector(i) < zs.vector(i)) 1 else 0
  private def phiIndex(zs: QuadTree[T]) = S1phi(zs).map(i => math.pow(2, k - i -1).toInt).sum
  private def hasSon(i: Int) = sons(i).isDefined
  private def son(i: Int) = sons(i).get
  private def sonVector(i: Int) = son(i).vector

  /**
   * Insert a new vector.
   * If this vector is already dominated by some other vectors, it is discarded. If this vector dominates other vectors already presents, those dominated vectors are discarded.
   * Complexity O(n) with n the number of vectors in the tree.
   */
  def insert(vector: Array[T]) {
    process(new QuadTree(vector))
  }
  
  def print() {
    println(vector.mkString(","))
    sons.filter(_.isDefined).foreach{case Some(t) => t.print()}
  }
  
  /**
   * Number of non dominated vectors in the tree
   */
  def size: Int = 1 + sons.flatten.map(s => s.size).sum

  
  /**
   * A set with all non dominated vectors in the tree
   */
  def toSet: Set[Array[T]] = sons.flatten.foldLeft(Set(vector))((s,x) => s union x.toSet)

  private def process(zs: QuadTree[T]) {
    val S0 = S0phi(zs)
    val S1 = S1phi(zs)
    val phiInd = phiIndex(zs)
    if (phiInd == 0) return // this dominates zs
    if (S0.forall(i => this(i) == zs(i))) {
      // replace since this is dominated by zs
      val oldSons = sons
      vector = zs.vector
      sons = zs.sons
      for (theta <- sonsFrom(1); if oldSons(theta).isDefined) {
        reconsider(oldSons(theta).get)
      }     
      return
    }
    for (theta <- sonsFrom(phiInd + 1); if hasSon(theta) && inS1(S1, theta)) {
      son(theta).test2(zs)
    }
    for (theta <- 1 until phiInd; if hasSon(theta) && inS0(S0, theta)) {
      son(theta).test1(zs)
    }
    sons(phiInd) match {
      case Some(zt: QuadTree[T]) =>
        zt.process(zs)
      case _ =>
        sons(phiInd) = Some(zs)
    }
  }

  // detects all vectors currently in the subtree dominated by zs
  private def test1(zs: QuadTree[T]) {
    val phiInd = phiIndex(zs)
    val S0 = S0phi(zs)
    if (S0.forall(i => this(i) == zs(i))) {
      // step 3
      // delete
      val theta: Int = sonsFrom(1) find (sons(_).isDefined) getOrElse (k)
      if (theta < k) {
        val oldSons = sons
        vector = son(theta).vector
        sons = son(theta).sons
        for (s <- sonsFrom(theta+1); if oldSons(s).isDefined) {
          reinsert(oldSons(s).get)
        }
      }
      test1(zs)
    } else {
      // step 4
      for (theta <- 1 until phiInd; if hasSon(theta) && inS0(S0, theta)) {
        son(theta).test1(zs)
      }
    }

  }
  // check if there are any vectors in the subtree that dominates zs
  private def test2(zs: QuadTree[T]) {
    val S0 = S0phi(zs)
    val S1 = S1phi(zs)
    val phiInd = phiIndex(zs)
    if (phiInd == 0) return // we are already sure zs is dominated by zr
    for (theta <- sonsFrom(phiInd); if hasSon(theta) && inS1(S1, theta)) {
      son(theta).test2(zs)
    }
  }

  // find the right position in the subtree at which to insert zs and it's successors
  private def reinsert(zs: QuadTree[T]) {
    for (theta <- sonsFrom(1); if zs.sons(theta).isDefined) {
      reinsert(zs.sons(theta).get)
    }
    val phiInd = phiIndex(zs)
    if (sons(phiInd).isDefined) {
      sons(phiInd).get.reinsert(zs)
    } else if (!sons(phiInd).isDefined) {
      sons(phiInd) = Some(zs)
    }
    
  }

  // the only difference with reinsert is that zs may be dominated by zr
  private def reconsider(zs: QuadTree[T]) {
    for (theta <- sonsFrom(1); if zs.sons(theta).isDefined) {
      reconsider(zs.sons(theta).get)
    }
    
    for (i <- sonsFrom(1)) {
      zs.sons(i) = None
    }
    val phiInd = phiIndex(zs)
    if (phiInd == 0) return
    if (sons(phiInd).isDefined) {
      sons(phiInd).get.reinsert(zs)
    } else if (!sons(phiInd).isDefined) {
      sons(phiInd) = Some(zs)
    }
    
  }
}

object IntQuadTree {
  def apply(dim: Int) = new QuadTree[Int](Array.fill(dim)(Int.MinValue))
}

object DoubleQuadTree {
  def apply(dim: Int) = new QuadTree[Double](Array.fill(dim)(Double.NegativeInfinity))
}