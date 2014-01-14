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
 * Reversible Boolean
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleBool(node: ReversibleContext, value: Boolean) extends ReversiblePointer[Boolean](node, value) {

  /**
   * Creates a reversible Boolean initialized to true
   * @param node
   */
  def this(node: ReversibleContext) = this(node, true)

  // I have to add it for Java compatibility
  // override def getValue(): Boolean = super.getValue()
}

object ReversibleBool {
  implicit def revBool2Bool(rb: ReversibleBool): Boolean = rb.getValue
}
