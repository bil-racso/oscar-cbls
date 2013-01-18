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

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._


/**
 * Balanced Academic Curriculum Problem
 * @author Pierre Schaus pschaus@gmail.com
 */
object BACP extends App{

    val lines = Source.fromFile("data/bacp/instances12/inst0.txt").getLines.reduceLeft(_ + " " + _)
    val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
    var index = 0
    def next() = {
      index += 1
      vals(index - 1)
    }

    val nbCourses = next()
    val courses = 0 until nbCourses
    val nbPeriods = next()
    val periods = 0 until nbPeriods
    val mincredit = next()
    val maxcredit = next()
    val nbPre = next()
    val credits = Array.fill(nbCourses)(next())
    val prerequisites = Array.fill(nbPre)((next(), next()))

    val cp = CPSolver()
    var x = Array.fill(nbCourses)(CPVarInt(cp,periods))
    val l = Array.fill(nbPeriods)(CPVarInt(cp,0 to credits.sum))
    val vari = CPVarInt(cp,0 to 10000000)



    cp.minimize(vari) subjectTo {
        cp.add(spread(l,credits.sum, vari))
        cp.add(binPacking(x, credits, l))
        for ((i,j) <- prerequisites) {
          cp.add(x(i) < x(j)) // precedence constraint
        } 
    } exploration {
        cp.binaryFirstFail(x,x => selectMin(periods)(x.hasValue(_))(l(_).min).get)
    } 
    
    cp.printStats

}
