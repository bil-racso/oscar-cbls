package oscar.cbls.lib.constraint

/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import oscar.cbls._
import oscar.cbls.lib.invariant.set.Cardinality

/**
 * implements \exists i, i \in set
 * @author gael.thouvenin@student.umons.ac.be
 */
case class Empty(set: SetValue) extends Constraint {
  private val content = EQ(Cardinality(set), 0L)

  override def violation(v: Value): IntValue = content.violation(v)

  override def violation: IntValue = content.violation
}
