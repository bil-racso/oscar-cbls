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

import oscar.util.tree.Node

/**
 * QuadTree able to maintain a set of non dominated vectors (assuming maximizations in all dimensions).
 * @author Pierre Schaus pschaus@gmail.com
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class QuadTree[T, E](var mooPoint: MOOPoint[T, E]) extends ParetoFront[T, E] {
  /** The number of dimensions in the evaluation space */
  private val k = mooPoint.nbEvaluations
  /** The number of sons a quad-tree can have */
  private val nbSons = math.pow(2, k).toInt
  /** The number of sons a non-dominated quad-tree can have */
  private val nbPossibleSons = math.pow(2, k).toInt - 2
  /** The sons of a quad-tree */
  private val sons = Array.fill[Option[QuadTree[T, E]]](nbSons)(None)
  /** The sequence of index from i to the max number of sons */
  private def sonsFrom(i: Int) = i to nbPossibleSons
  /** The successorship of a point compared to the root of this */
  private def successorshipArray(candidatePoint: MOOPoint[T, E]): Array[Int] = {
    Array.tabulate(k)(i => if (mooPoint.worseThan(i, candidatePoint)) 1 else 0)
  }
  /** The successorship value of a point compared to the root of this */
  private def successorshipValue(candidatePoint: MOOPoint[T, E]): Int = {
    var succVal = 0
    for(i <- 0 until k; if (mooPoint.worseThan(i, candidatePoint))) {
      succVal += math.pow(2, i).toInt
    }
    succVal
  }
  /** The set of indices for which the evaluation function for which point was lower or equal to the root of this */
  private def successorshipS0(candidatePoint: MOOPoint[T, E]) = for (i <- 0 until k; if mooPoint.betterOrEqualTo(i, candidatePoint)) yield i
  /** The set of indices for which the evaluation function for which point was higher than the root of this */
  private def successorshipS1(candidatePoint: MOOPoint[T, E]) = for (i <- 0 until k; if mooPoint.worseThan(i, candidatePoint)) yield i
  /** The set of indices for which the evaluation function for which point was lower or equal to the root of this */
  private def successorshipS0(phiArray: Array[Int]) = for (i <- 0 until k; if phiArray(i) == 0) yield i
  /** The set of indices for which the evaluation function for which point was higher than the root of this */
  private def successorshipS1(phiArray: Array[Int]) = for (i <- 0 until k; if phiArray(i) == 1) yield i
  /** True if this contains a subtree rooted at index i */
  private def hasSon(i: Int) = sons(i).isDefined
  /** True if this contains subtrees */
  private def hasSon: Boolean = !sonsFrom(1).forall(!hasSon(_))
  /** Gets the son which labels is i */
  private def son(i: Int) = sons(i).get
  /** Gets the son with the lowest phi index */
  private def definedSons: Array[QuadTree[T, E]] = {
    sons.filter(e => e match{case Some(a) => true case None => false}).map(e => e.get).toArray
  }
  /** Checks if the index i is in S0 */
  private def inS0(phi: Int, i: Int): Boolean = phi >> i == 0
  /** Checks if the index i is in S1 */
  private def inS1(phi: Int, i: Int): Boolean = phi >> i == 1
  /** Checks if phi is in S0 */
  private def inS0(S0: Iterable[Int], phi: Int): Boolean = S0.forall(inS0(phi, _))
  /** Checks if phi is in S1 */
  private def inS1(S1: Iterable[Int], phi: Int): Boolean = S1.forall(inS1(phi, _))
  /** Checks if phi is strictly in S0 */
  private def subS0(S0: Iterable[Int], phi: Int): Boolean = {
    var rest = math.pow(2, k).toInt - 1
    for (index <- S0) {
      rest -= math.pow(2, index).toInt
    }
    inS0(S0, phi) && rest != phi
  }
  /** Checks if phi is strictly in S1 */
  private def subS1(S1: Iterable[Int], phi: Int): Boolean = {
    var rest = 0
    for (index <- S1) {
      rest += math.pow(2, index).toInt
    }
    inS1(S1, phi) && rest != phi
  }
  private def phiToArray(phi: Int): Array[Int] = {
    var rest = phi
    val tab = Array.fill(k)(0)
    for (i <- k - 1 to 0 by -1) {
      val power = math.pow(2, i).toInt
      if (rest / power != 0) {
        tab(i) = 1
        rest -= power
      }
    }
    tab
  }
  private def binaryStr(num: Int): String = {
    var repr = ""
    var rest = num
    for (i <- (k - 1) to 0 by (-1)) {
      val power = math.pow(2, i).toInt
      if (rest / power != 0) {
        repr += "1"
        rest -= power
      }
      else {
        repr += "0"
      }
    }
    repr.reverse
  }
  
  def insert(candidatePoint: MOOPoint[T, E]) {
    val psi = successorshipValue(candidatePoint)
    // occurs when candidate is dominated
    if (psi == 0){
      return
    }
    val S0 = successorshipS0(candidatePoint)
    // occurs when candidate is equivalent to the root
    if (S0.forall(i => mooPoint.evaluationEquivalentTo(i, candidatePoint))) {
      replace(candidatePoint)
      return
    }
    val S1 = successorshipS1(candidatePoint)
    // checks in all other branches if candidate is dominated
    for (phi <- sonsFrom(psi + 1); if hasSon(phi) && subS1(S1, phi)) {
      if (son(phi).doesDominate(candidatePoint)) return
    }
    // checks in a point in other branches is dominated by candidate
    for (phi <- 1 until psi; if hasSon(phi) && subS0(S0, phi)) {
      son(phi).isDominatedBy(this, candidatePoint)
    }
    // if no psi-son, we insert it directly as the psi-son
    if (!hasSon(psi)) {
      sons(psi) = Some(QuadTree(candidatePoint))
    }
    // the psi-son exists thus we recursively insert it inside this one
    else {
      son(psi).insert(candidatePoint)
    }
  }
  
  /** Replacement of the root by the candidate because the root of this is dominated. */
  def replace(candidatePoint: MOOPoint[T, E]) {
    mooPoint = candidatePoint
    for (phi <- sonsFrom(1); if hasSon(phi)) {
      reconsider(this, phi, phi)
    }
  }
  
  /** Sees if the subtree can be kept at the same position in the tree */
  def reconsider(newAncestor: QuadTree[T, E], psiIndex: Int, formerPhi: Int) {
    val subTree = son(psiIndex)
    val psiForNew = newAncestor.successorshipValue(subTree.mooPoint)
    // occurs when subtree is dominated
    if (psiForNew == 0) {
      sons(psiIndex) = None
      for (phi <- sonsFrom(1); if subTree.hasSon(phi)) {
        newAncestor.reinsert(newAncestor, subTree.son(phi))
      }
    }
    else if(psiForNew != formerPhi) {
      sons(psiIndex) = None
      newAncestor.reinsert(newAncestor, subTree)
    }
    else {
      for (phi <- sonsFrom(1); if subTree.hasSon(phi)) {
        subTree.reconsider(newAncestor, phi, formerPhi)
      }
    }
  }
  
  def reinsert(newAncestor: QuadTree[T, E], subTree: QuadTree[T, E]) {
    val psi = successorshipValue(subTree.mooPoint)
    if (psi == 0) {
      for (phi <- sonsFrom(1); if subTree.hasSon(phi)) {
        newAncestor.reinsert(newAncestor, subTree.son(phi))
      }
      return
    }
    if (!hasSon(psi)) {
      sons(psi) = Some(subTree)
      for(phi <- sonsFrom(1); if subTree.hasSon(phi)) {
        if (newAncestor == this) {
          subTree.reconsider(newAncestor, phi, phi)
        }
        else {
          val newSubTree = subTree.son(phi)
          subTree.sons(phi) = None
          newAncestor.reinsert(newAncestor, newSubTree)
        }
      }
    }
    else {
      son(psi).reinsert(newAncestor, subTree)
    }
  }
  
  def doesDominate(candidatePoint: MOOPoint[T, E]): Boolean = {
    val psi = successorshipValue(candidatePoint)
    val S0 = successorshipS0(candidatePoint)
    if (psi == 0 || S0.forall(i => mooPoint.evaluationEquivalentTo(i, candidatePoint))) true
    else {
      for (phi <- sonsFrom(1); if hasSon(phi) && subS0(S0, phi)) {
        if (son(phi).doesDominate(candidatePoint)) {
          return true
        }
      }
      false
    }
  }
  
  def isDominatedBy(parentTree: QuadTree[T, E], candidatePoint: MOOPoint[T, E]) {
    val psi = successorshipValue(candidatePoint)
    val S1 = successorshipS1(candidatePoint)
    if (psi == 0) {
      deletedBy(parentTree, candidatePoint)
    }
    else {
      for (phi <- sonsFrom(1); if hasSon(phi) && subS1(S1, phi)) {
        son(phi).isDominatedBy(this, candidatePoint)
      }
    }
  }
  
  def deletedBy(parentTree: QuadTree[T, E], candidatePoint: MOOPoint[T, E]) {
    val psi = parentTree.successorshipValue(this.mooPoint)
    if (!hasSon) {
      parentTree.sons(psi) = None
    }
    else {
      val formerDefinedSons = definedSons
      // Splitting the sons in two categories
      val dominatedSons = definedSons.filter(son => candidatePoint.dominates(son.mooPoint))
      val nonDominatedSons = definedSons.filter(son => !candidatePoint.dominates(son.mooPoint))
      if (!nonDominatedSons.isEmpty) {
        val firstSon = nonDominatedSons(0)
        parentTree.sons(psi) = Some(firstSon)
        for (phi <- sonsFrom(1); if firstSon.hasSon(phi)) {
          firstSon.son(phi).isDominatedBy(parentTree, candidatePoint)
        }
        for (otherSon <- nonDominatedSons.drop(1)) {
          val newPhi = firstSon.successorshipValue(otherSon.mooPoint)
          firstSon.reinsert(firstSon, otherSon)
          otherSon.isDominatedBy(parentTree, candidatePoint)
        }
        for (dominatedSon <- dominatedSons) {
          for (phi <- sonsFrom(1); if dominatedSon.hasSon(phi)) {
            val otherSon = dominatedSon.son(phi)
            val newPhi = firstSon.successorshipValue(otherSon.mooPoint)
            firstSon.reinsert(firstSon, otherSon)
            otherSon.isDominatedBy(parentTree, candidatePoint)
          }
        }
      }
      else {
        for (dominatedSon <- dominatedSons) {
          for (phi <- sonsFrom(1); if dominatedSon.hasSon(phi)) {
            val otherSon = dominatedSon.son(phi)
            val newPhi = parentTree.successorshipValue(otherSon.mooPoint)
            parentTree.reinsert(parentTree, otherSon)
          }
        }
      }
      
    }
  }
  
  /**
   * A Node-like representation of a quad-tree used to represent it visually
   */
  def toNode: Node[String] = {
    if (!hasSon)
      Node(mooPoint.toString)
    else {
      var sonsList = List[Node[String]]()
      var edgeLabelsList = List[String]()
      for(i <- sonsFrom(0)) {
        if(hasSon(i)) {
          sonsList :+= son(i).toNode
          edgeLabelsList :+= binaryStr(i)
        }
      }
      Node(mooPoint.toString, sonsList, edgeLabelsList)
    }
  }
  
  //TODO
  def nbPointsDominating(candidatePoint: MOOPoint[T, E]): Int = {
    0
  }
  
  //TODO
  def nbPointsDominated(candidatePoint: MOOPoint[T, E]): Int = {
    0
  }
  
  //TODO
  def score(point: MOOPoint[T, E]): Int = {
    0
  }
  
  def toSet: Set[MOOPoint[T, E]] = sons.flatten.foldLeft(Set(mooPoint))((s,x) => s union x.toSet)
  
  override def toString: String = {
    def relString(qt: QuadTree[T, E], levelAcc: Int): String = {
      var repr = qt.mooPoint.toString
      for (phi <- 0 until nbSons; if qt.hasSon(phi)) {
        repr += "\n" + " | " * levelAcc + binaryStr(phi) + ": " + relString(qt.son(phi), levelAcc + 1)
      }
      repr
    }
    relString(this, 1)
  }
  
  def size: Int = 1 + sons.flatten.map(s => s.size).sum
}

/** Factory for QuadTree instances. */
object QuadTree {
  def apply[T, E](mooPoint: MOOPoint[T, E]) = new QuadTree(mooPoint)
}