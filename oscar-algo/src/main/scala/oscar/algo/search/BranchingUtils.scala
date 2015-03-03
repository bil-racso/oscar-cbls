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

package oscar.algo.search

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
trait BranchingUtils {
  
  type Alternative = Function0[Any]
  
  def branch(left: => Any)(right: => Any): Seq[Alternative] = Seq(() => left,() => right)
  
  def branchOne(action: => Any) = Seq(() => action)
  
  def branchAll[A](indexes: Seq[A])(f: A => Any): Seq[Alternative] = {
    indexes.map(i => () => f(i))
  }
  
  val noAlternative = Seq[Alternative]()
}
