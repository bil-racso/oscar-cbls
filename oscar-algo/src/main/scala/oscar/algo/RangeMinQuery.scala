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

package oscar.algo

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class RangeMinQuery(values: Array[Int]) {

    val n = values.length
    val m = Array.fill(n,n)(Int.MinValue ) 
    for (i <- 0 until n) m(i)(i) = i
    for (i <- 0 until n; j <- i+1 until n) {
      if (values(m(i)(j-1)) < values(j)) {
        m(i)(j) = m(i)(j-1)
      } else {
        m(i)(j) = j
      }
    }
    
    
    /**
     * @param
     * @return the index of the minimum value between indices i and j (included).
     */
    def apply(i: Int, j: Int): Int = {
      assert(i < j)
      m(i min j)(j max i)
    }
  
}