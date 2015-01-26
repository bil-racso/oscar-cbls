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


class ReversibleIntWithCacheTrailEntry(reversible: ReversibleIntWithCache, value: Int) extends TrailEntry {
  @inline override final def restore(): Unit = reversible.restore(value)
}

/**
 * Similar as ReversibleInt except that the TrailEntry's 
 * from 0 to maxSize such that they don't need 
 * to be created more than once (avoid garbage collection).
 * 
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleIntWithCache(node: ReversibleContext, value: Int, maxSize: Int) extends ReversiblePointer[Int](node, value) {

  private[this] val cachedEntries = Array.tabulate(maxSize)(i => new ReversibleIntWithCacheTrailEntry(this, i))
  
  @inline final override def trailEntry = cachedEntries(pointer)
  
  /** Increments the reversible integer by one */
  @inline final def incr(): Int = {
    trail()
    pointer += 1
    pointer
  }

  /** Decrements the reversible integer by one */
  @inline final def decr(): Int = {
    trail()
    pointer -= 1
    pointer
  }

  /** Increments the reversible integer by i */
  @inline final def +=(i: Int): Int = {
    trail()
    pointer += i
    pointer
  }

  /** Decrements the reversible integer by i */
  @inline final def -=(i: Int): Int = {
    trail()
    pointer -= i
    pointer
  }
}

object ReversibleIntWithCache {
  def apply(value: Int, n: Int)(implicit context: ReversibleContext) = new ReversibleIntWithCache(context, value, n)
}
