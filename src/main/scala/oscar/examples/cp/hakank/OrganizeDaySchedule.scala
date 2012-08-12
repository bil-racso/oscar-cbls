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

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object OrganizeDaySchedule {


  def main(args: Array[String]) {

    //
    // data
    //
    val n = 4

    val Activities = 0 until n
   
    val Array(work, mail, shop, bank) = (0 to 3).toArray
    val tasks = Array(work, mail, shop, bank)
    val tasks_str = Array("Work", "Mail", "Shop", "Bank")

    val durations = Array(4,1,2,1)
    val resources = Array(1,1,1,1)

    // precedences: 
    // task(t,0) must be finished before task(t, 1)
    val before_tasks = Array(Array(bank, shop),
                             Array(mail, work))

    val capa = 1

    // the valid times of the day
    val begin = 9
    val end   = 17

    val horizon = end - begin + 1
    println("horizon: " + horizon)
    val cp = CPScheduler(horizon)

    //
    // variables
    //
    val resource = CumulativeResource(cp, capa, "OrganizeDay")
    val activities = Array.tabulate(n)(a=>Activity(cp, durations(a)))
    for(a <- Activities) {
      activities(a) needs 1 ofResource resource
    }

    val makespan = cp.makespan

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      // precedences
      for(t <- 0 until before_tasks.length) {
        cp.add(activities(before_tasks(t)(0)) precedes activities(before_tasks(t)(1)))
      }

      // cp.add(activities(work).start >= 11)
      cp.add(activities(work).start >= 11 - begin)
      cp.add(makespan <= end-begin)

    } exploration {
       
      // cp.setTimes(cp.activities)
      cp.binaryFirstFail(cp.activities)

      println("makespan : " + makespan)
      println("criticality: " + resource.criticality)
      println(activities.map(a=>"Activity " + tasks_str(a.id) + ": " + (a.start.value+begin) + " --" + a.dur + "h -- " + (a.end.value+begin)).mkString("\n"))
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
