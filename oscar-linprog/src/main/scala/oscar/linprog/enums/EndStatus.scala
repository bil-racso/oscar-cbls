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

package oscar.linprog.enums

sealed abstract class EndStatus(val name: String) {
  override def toString: String = name
}

case object Solution extends EndStatus("SOLUTION_FOUND")
case object Unbounded extends EndStatus("UNBOUNDED")
case object Infeasible extends EndStatus("INFEASIBLE")
case object NoSolution extends EndStatus("NO_SOLUTION")

case class NoSolutionFound(endStatus: EndStatus) extends Exception(s"No solution found to the problem, end status is $endStatus")
