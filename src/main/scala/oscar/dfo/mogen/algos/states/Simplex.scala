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
package oscar.dfo.mogen.algos.states

import oscar.util.mo.MOOPoint
import oscar.util.mo.MOOComparator
import oscar.util.mo.RandomGenerator

trait Simplex[E] {

  val simplex: Array[MOOPoint[E]]
  
  def simplexSize = simplex.length
  
  var bestPoint: MOOPoint[E]

  def getBestPoint = bestPoint
  
  def nbCoordinates = simplex(0).nbCoordinates
  
  def worstPoint = simplex(simplexSize - 1)

  def orderSimplex(comparator: MOOComparator[E]) = {
    simplex.sortWith((point1, point2) => ((point1 == bestPoint) || (comparator.isEquivalent(point1, point2) && 0.5 > RandomGenerator.nextDouble) || comparator.dominates(point1, point2)))
  }

  def getPoints: List[MOOPoint[E]] = simplex.toList

  def arraySum(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) + ar2(i))
  
  def arrayDiff(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) - ar2(i))
  
  def arrayProd(ar: Array[Double], factor: Double): Array[Double] = Array.tabulate(ar.length)(i => factor * ar(i))
}
