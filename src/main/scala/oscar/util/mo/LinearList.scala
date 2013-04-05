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
import scala.Array.canBuildFrom
import scala.collection.mutable.DoubleLinkedList

/** LinearList able to maintain a set of non dominated points (assuming maximizations in all dimensions).
  * 
  * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com */
class LinearList[T, E <% Ordered[E]](private var mooPoint: MOOPoint[T, E]) extends ParetoFront[T, E]{
  
  /** The list containing the non-dominated points */
  private var archive = List(mooPoint);
  
  /** Insert a new point in the archive.
    * 
    * If this point is already dominated by some other points, it is discarded. If this point dominates other
    * points already presents, those dominated points are discarded. Complexity O(n) with n the number of
    * points in the linear list. */
  def insert(mooPoint: MOOPoint[T, E], comparator: MOOComparator[T, E]) {
    var newList = List[MOOPoint[T, E]]()
    def getFiltered(l: List[MOOPoint[T, E]]): List[MOOPoint[T, E]] = {
      if (l.isEmpty) {
        List[MOOPoint[T, E]](mooPoint)
      }
      else {
        if (comparator.isEquivalent(mooPoint, l.head)) {
          return l
        }
        else {
          if (comparator.dominates(mooPoint, l.head)) getFiltered(l.tail)
          else if (comparator.dominated(mooPoint, l.head)) l
          else l.head :: getFiltered(l.tail)
	    }
      }
    }
    archive = getFiltered(archive)
  }
  
  def nbPointsDominating(candidatePoint: MOOPoint[T, E], comparator: MOOComparator[T, E]): Int = {
    var domCount = 0
    for (point <- archive) {
      if (comparator.dominated(point, candidatePoint))
        domCount += 1
    }
    domCount
  }
  
  def nbPointsDominated(candidatePoint: MOOPoint[T, E], comparator: MOOComparator[T, E]): Int = {
    var domCount = 0
    for (point <- archive) {
      if (comparator.dominates(candidatePoint, point))
        domCount += 1
    }
    domCount
  }
  
  def score(point: MOOPoint[T, E], comparator: MOOComparator[T, E]): Int = {
    var score = 0
    for (p <- archive) {
      if (comparator.dominates(point, p)) score += 1
      else if (comparator.dominated(point, p)) score -= 1
    }
    score
  }
  
  def size: Int = archive.size
  
  def toSet: Set[MOOPoint[T, E]] = archive.toSet
  
  /** Displays the current set of non-dominated points. */
  def print() {
    for(element <- archive)
      println(element.toString)
  }
}

/** Factory for LinearList instances. */
object LinearList {
  def apply[T, E <% Ordered[E]](mooPoint: MOOPoint[T, E]): LinearList[T, E] = new LinearList(mooPoint)
}