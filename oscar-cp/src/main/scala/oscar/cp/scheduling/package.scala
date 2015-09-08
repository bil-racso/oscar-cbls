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

package oscar.cp

import oscar.cp.core.CPOutcome._
import oscar.cp.core._
package object scheduling {

  /**
   * prune such that activity 1 << activity 2
   */
  def precedes(s1: CPIntVar, d1: CPIntVar, e1: CPIntVar, s2: CPIntVar, d2: CPIntVar, e2: CPIntVar): CPOutcome = { 
      if (s2.updateMin(e1.min) == Failure) return Failure
      if (e1.updateMax(s2.max) == Failure) return Failure
      Suspend
  }

  /**
   * ensure s+d = e
   */
  def update(s: CPIntVar, d: CPIntVar, e: CPIntVar): CPOutcome = {
    // end <= start
    if (e.updateMin(s.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (s.updateMax(e.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } // end = start + dur
    else if (e.updateMax(s.max + d.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (e.updateMin(s.min + d.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } // start = end - dur
    else if (s.updateMax(e.max - d.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (s.updateMin(e.min - d.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } // dur = end - start
    else if (d.updateMax(e.max - s.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (d.updateMin(e.min - s.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else CPOutcome.Suspend
  }
  

}