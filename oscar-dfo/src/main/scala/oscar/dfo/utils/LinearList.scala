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

package oscar.dfo.utils

import scala.math.Numeric.Implicits._

/**
 * LinearList able to maintain a set of non dominated points (assuming maximizations in all dimensions).
 *
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class LinearList[E <% Ordered[E]](initialPoints: List[ArchiveElement[E]]) extends ParetoFront[E] {

  /** The list containing the non-dominated points */
  private var archive = List[ArchiveElement[E]](initialPoints: _*);

  /**
   * Insert a new point in the archive.
   *
   * If this point is already dominated by some other points, it is discarded. If this point dominates other
   * points already presents, those dominated points are discarded. Complexity O(n) with n the number of
   * points in the linear list.
   */
  def insert(newElement: ArchiveElement[E], comparator: MOOComparator[E]): Boolean = {
    var newElemIsInserted = false
    def getFiltered(l: List[ArchiveElement[E]]): List[ArchiveElement[E]] = {
      if (l.isEmpty) {
        newElemIsInserted = true
        List[ArchiveElement[E]](newElement)
      } else {
        if (comparator.hasEqualEvals(newElement.getMOOPoint, l.head.getMOOPoint)) return getFiltered(l.tail)
        else {
          if (comparator.dominates(newElement.getMOOPoint, l.head.getMOOPoint)) getFiltered(l.tail)
          else if (comparator.dominated(newElement.getMOOPoint, l.head.getMOOPoint)) l
          else l.head :: getFiltered(l.tail)
        }
      }
    }
    archive = getFiltered(archive)
    return newElemIsInserted
  }

  def contains(mooPoint: MOOPoint[E]): Boolean = archive.exists(elem => elem.getMOOPoint.equals(mooPoint))

  def contains(element: ArchiveElement[E]): Boolean = archive.contains(element)

  def head: ArchiveElement[E] = archive.head

  def isEmpty: Boolean = archive.isEmpty

  def nbPointsDominating(candidatePoint: MOOPoint[E], comparator: MOOComparator[E]): Int = {
    var domCount = 0
    for (elem <- archive) {
      if (comparator.dominated(elem.getMOOPoint, candidatePoint))
        domCount += 1
    }
    domCount
  }

  def nbPointsDominated(candidatePoint: MOOPoint[E], comparator: MOOComparator[E]): Int = {
    var domCount = 0
    for (elem <- archive) {
      if (comparator.dominates(candidatePoint, elem.getMOOPoint))
        domCount += 1
    }
    domCount
  }

  def score(point: MOOPoint[E], mooPointCmpRaw: MOOComparator.MOOPointCmpRaw[E]): Int = {
    var score = 0
    for (elem <- archive) {
      if (mooPointCmpRaw(point, elem.getMOOPoint)) score += 1
      else if (mooPointCmpRaw(elem.getMOOPoint, point)) score -= 1
    }
    score
  }

  def randomElement: ArchiveElement[E] = archive(RandomGenerator.nextInt(archive.length))

  /** Removes the specified element from the Pareto front */
  def removeElement(formerElement: ArchiveElement[E]): Boolean = {
    val tmpArch = archive.filter(elem => elem != formerElement)
    if (archive == tmpArch) false
    else {
      archive = tmpArch
      true
    }
  }

  def size: Int = archive.size

  def toSet: Set[MOOPoint[E]] = archive.map(elem => elem.getMOOPoint).toSet

  def toList: List[MOOPoint[E]] = archive.map(elem => elem.getMOOPoint)

  /** Displays the current set of non-dominated points. */
  def print() {
    for (element <- archive)
      println(element.toString)
  }
}

/** Factory for LinearList instances. */
object LinearList {
  def apply[E <% Ordered[E]](initialPoints: List[ArchiveElement[E]]) = new LinearList(initialPoints)
  def apply[E <% Ordered[E]](initialPoint: ArchiveElement[E]) = new LinearList(List(initialPoint))
  def apply[E <% Ordered[E]]() = new LinearList(List[ArchiveElement[E]]())
}
