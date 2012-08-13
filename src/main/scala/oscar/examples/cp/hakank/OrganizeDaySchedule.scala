/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import oscar.cp.scheduling._
import scala.io.Source._
import scala.math._

/*

  Organizing a day in Oscar.

  Simple scheduling problem.

  Problem formulation from ECLiPSe:
   - Slides on (Finite Domain) Constraint Logic Programming, page 38f
   - http://eclipse-clp.org/reports/eclipse.ppt
 
  Cf OrganizeDay.scala
  This version use the Scheduling API.

Solutions from OrganizeDay.scala:
work: 11 -- 4h -- 15
mail: 9 -- 1h -- 10
shop: 15 -- 2h -- 17
bank: 10 -- 1h -- 11

work: 11 -- 4h -- 15
mail: 10 -- 1h -- 11
shop: 15 -- 2h -- 17
bank: 9 -- 1h -- 10

work: 13 -- 4h -- 17
mail: 12 -- 1h -- 13
shop: 10 -- 2h -- 12
bank: 9 -- 1h -- 10

work: 13 -- 4h -- 17
mail: 9 -- 1h -- 10
shop: 11 -- 2h -- 13
bank: 10 -- 1h -- 11

work: 13 -- 4h -- 17
mail: 10 -- 1h -- 11
shop: 11 -- 2h -- 13
bank: 9 -- 1h -- 10


This model yields 9 solutions:

Activity Work: 13 -- 4h -- 17
Activity Mail: 10 -- 1h -- 11
Activity Shop: 11 -- 2h -- 13
Activity Bank: 9 -- 1h -- 10

Activity Work: 11 -- 4h -- 15
Activity Mail: 10 -- 1h -- 11
Activity Shop: 15 -- 2h -- 17
Activity Bank: 9 -- 1h -- 10

Activity Work: 13 -- 4h -- 17
Activity Mail: 12 -- 1h -- 13
Activity Shop: 10 -- 2h -- 12
Activity Bank: 9 -- 1h -- 10

Activity Work: 11 -- 4h -- 15
Activity Mail: 10 -- 1h -- 11
Activity Shop: 15 -- 2h -- 17
Activity Bank: 9 -- 1h -- 10

Activity Work: 13 -- 4h -- 17
Activity Mail: 9 -- 1h -- 10
Activity Shop: 11 -- 2h -- 13
Activity Bank: 10 -- 1h -- 11

Activity Work: 11 -- 4h -- 15
Activity Mail: 9 -- 1h -- 10
Activity Shop: 15 -- 2h -- 17
Activity Bank: 10 -- 1h -- 11

Activity Work: 13 -- 4h -- 17
Activity Mail: 9 -- 1h -- 10
Activity Shop: 11 -- 2h -- 13
Activity Bank: 10 -- 1h -- 11

Activity Work: 11 -- 4h -- 15
Activity Mail: 9 -- 1h -- 10
Activity Shop: 15 -- 2h -- 17
Activity Bank: 10 -- 1h -- 11

Activity Work: 13 -- 4h -- 17
Activity Mail: 12 -- 1h -- 13
Activity Shop: 10 -- 2h -- 12
Activity Bank: 9 -- 1h -- 10

      Aha, 4 of these 9 solutions are duplicates:

# Occurrences Schedule
--------------------
      2       Activity Work: 11 -- 4h -- 15 Activity Mail: 10 -- 1h -- 11 Activity Shop: 15 -- 2h -- 17 Activity Bank: 9 -- 1h -- 10
      2       Activity Work: 11 -- 4h -- 15 Activity Mail: 9 -- 1h -- 10 Activity Shop: 15 -- 2h -- 17 Activity Bank: 10 -- 1h -- 11
      1       Activity Work: 13 -- 4h -- 17 Activity Mail: 10 -- 1h -- 11 Activity Shop: 11 -- 2h -- 13 Activity Bank: 9 -- 1h -- 10
      2       Activity Work: 13 -- 4h -- 17 Activity Mail: 12 -- 1h -- 13 Activity Shop: 10 -- 2h -- 12 Activity Bank: 9 -- 1h -- 10
      2       Activity Work: 13 -- 4h -- 17 Activity Mail: 9 -- 1h -- 10 Activity Shop: 11 -- 2h -- 13 Activity Bank: 10 -- 1h -- 11


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object OrganizeDaySchedule extends App {
		
	// the valid times of the day
	val begin = 9
	val end = 17

	val horizon = end - begin
	val cp = CPScheduler(horizon)
	
	println("horizon: " + horizon)

	val work = Activity(cp, 4, "Work")
	val mail = Activity(cp, 1, "Mail")
	val shop = Activity(cp, 2, "Shop")
	val bank = Activity(cp, 1, "Bank")

	val resource   = UnitResource(cp)
	val activities = cp.activities
	
	for (activity <- activities)
		activity needs resource

	var numSols = 0

	cp.solveAll subjectTo {

		// precedences
		cp.add(bank precedes shop)
		cp.add(mail precedes work)

		cp.add(work.start >= 11 - begin)

	} exploration {

		cp.binaryFirstFail(activities)

		println(activities.map(a => a.name + ": " + (a.start.value + begin) + " --" + a.dur + "h -- " + (a.end.value + begin)).mkString("\n"))
		println()

		numSols += 1
	}

	println("\nIt was " + numSols + " solutions.")
	cp.printStats()
}