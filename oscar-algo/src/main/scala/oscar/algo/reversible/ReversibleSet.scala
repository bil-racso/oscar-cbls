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
package oscar.algo.reversible;

/**
 * Reversible Set
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSet(context: ReversibleContext) extends Iterable[Int] {
  
  private val set = scala.collection.mutable.Set[Int]()

  def add(v: Int) = {
    context.trail {
     set.remove(v) 
    }
    set.add(v)
  }
  
  def remove(v: Int) = {
    context.trail {
     set.add(v) 
    }
    set.remove(v)
  }  
  
  def iterator: Iterator[Int] = set.iterator
}