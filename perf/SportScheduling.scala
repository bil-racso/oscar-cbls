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

import oscar.cp._


/**
 * The problem is to schedule an even number n of teams over n/2 periods and n - 1 weeks, 
 * under the following constraints: <br>
 *  - Each team must play against every other team <br>
 *  - A team plays exactly one game per week  <br>
 *  - A team can play at most twice in the same period <br>
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object SportScheduling {
  
  def main(args: Array[String]) {

    val n = 14
    val nbPeriods = n / 2
    val Periods = 0 until nbPeriods
    val nbTeams = n
    val Teams = 0 until nbTeams
    val nbWeeks = n - 1
    val Weeks = 0 until nbWeeks
    val Homes = 0 to 1 // 0/1 for home/away game

    val cp = CPSolver()
    cp.silent = true
    val team = Array.tabulate(nbPeriods, nbWeeks, 2)((p, w, h) => CPIntVar(cp, 0 until nbTeams))
    val game = Array.tabulate(nbPeriods, nbWeeks)((p, w) => CPIntVar(cp, 0 until (n * n - 1)))
    val tuples = for (i <- Teams; j <- i + 1 until nbTeams) yield (i, j, i * nbWeeks + j - 1)
    
    def printSol() {
      println("---------games---------")
      Periods.foreach {
        p => println(Weeks.map(w => game(p)(w).getValue).mkString("\t"))
      }
      println("---------teams---------")
      Periods.foreach {
        p => println(Weeks.map(w => (team(p)(w)(0).getValue, team(p)(w)(1).getValue)).mkString("\t"))
      }
      println("\n")
    }

    cp.solve subjectTo {
      // make the link between the team and game variables
      for (p <- Periods; w <- Weeks) {
        cp.add(table(team(p)(w)(0), team(p)(w)(1), game(p)(w), tuples))
      }
      // a team plays exactly one game per week
      for (w <- Weeks) {
        val teamw = for (p <- Periods; h <- Homes) yield team(p)(w)(h)
        cp.add(allDifferent(teamw), Strong)
      }
      // every team plays against every other team
      cp.add(allDifferent(game.flatten), Strong)
      // a team can play at most twice in the same period
      for (p <- Periods)
        cp.add(gcc(team(p).flatten, Teams, 1, 2),Strong)
    } search {
      binaryFirstFail(game.flatten.toSeq)
    } start(1)


  }

}
