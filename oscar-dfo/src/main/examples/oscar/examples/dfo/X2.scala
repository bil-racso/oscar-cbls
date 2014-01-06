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
package oscar.examples.dfo


import oscar.dfo.singleobjective.algos._
import oscar.dfo.utils._

object X2 extends App {

  // returns the array with objectives sum(i) x(i)*x(i) 
  def f1(x: Array[Double]): Array[Double] = Array(x.foldLeft(0.0)((res, element) => res + element * element))

  // tells whether objective in a1 dominates objective in a2
  def singleObjCompare(a1: Array[Double], a2: Array[Double]) = a1(0) < a2(0)

  val domain2D = Array.fill(2)(Interval(-100.0, 100.0))
  val startPoint2D = Array.fill(2) { 42.0 }
  val nm = NelderMead(f1, 1, singleObjCompare, startPoint2D, domain2D)
  // optimize 100 times with pseudo random restarts (scrambled Halton sequence)
  val nmAns = nm.sampledOptimize(100, math.pow(10, -2))
  println("Nelder-Mead: objective:" + nmAns._2(0) + " coordinate:" + (nmAns._1).mkString(",") + "\nAverage number of evaluations: " + nm.evalCount / 200.0)

}
