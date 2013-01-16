package oscar.examples.cbls

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.scheduling._

/**a simple model of Reagan president of USA
 * he is partly multitask, can do two things at the same time, except eating, which requires his full attention
 * he needs to sleep 2H, eat 30', chew 45' and think 3H
 * he cannot sleep before having eaten
 */
object Reagan extends App {
  val model = new Model(false, true, false)
  val planning = new Planning(model, 31)
  val solver = new IFlatIRelax(planning)

  val Reagan = CumulativeResource(planning, 3, "Reagan")

  val Manger = Task(2, planning, "eat")
  Manger uses 2 ofResource Reagan

  val Dormir = Task(8, planning, "sleep")
  Dormir uses 1 ofResource Reagan

  val Reflechir = Task(12, planning, "think")
  Reflechir uses 1 ofResource Reagan

  val Chiquer = Task(3, planning, "chew")
  Chiquer uses 1 ofResource Reagan

  val Parler = Task(3, planning, "speak")
  Parler uses 3 ofResource Reagan

  val Boire = Task(3, planning, "drink")
  Boire uses 3 ofResource Reagan

  val Digérer = SuperTask(Manger, Dormir, "digest")

  Reflechir precedes Boire
  Manger precedes Dormir
  Chiquer precedes Parler

  planning.close()
  model.close(false)
 // println(model.dumpToDot(true, true))
  solver.Solve(10, 4, solver.WorseFirst(), 2, 50)

  println(planning.toAsciiArt)
}
