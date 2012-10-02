/** *****************************************************************************
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
import oscar.visual._
import scala.collection.JavaConversions._
import scala.io.Source
import java.lang._
import java.awt.Color

/**
  * tsp model with visualization:
  * given a distance matrix between 20 cities,
  * find the shortest tour visiting each city exactly once.
  *
  * @author Pierre Schaus pschaus@gmail.com
  */
object TSPVisual extends App {

	val n = 18
	val Cities = 0 until n

	val rand = new scala.util.Random(0)
	val coord = Array.tabulate(n)(i => (100 + rand.nextInt(400), rand.nextInt(400)))

	def getDist(p1: (Int, Int), p2: (Int, Int)): Int = {
		val dx = p2._1 - p1._1
		val dy = p2._2 - p1._2
		math.sqrt(dx * dx + dy * dy).asInstanceOf[Int]
	}

	val distMatrix = Array.tabulate(n, n)((i, j) => getDist(coord(i), coord(j)))

	println(distMatrix)

	val cp = new CPSolver()
	//array of successors
	val succ = Array.tabulate(n)(_ => CPVarInt(cp, 0 until n))
	//total distance
	val dist = CPVarInt(cp, 0 to distMatrix.flatten.sum)

	val visu = new VisualTour(coord, succ, dist, "TSP")

	cp.minimize(dist) subjectTo {
		cp.add(circuit(succ), Strong) //ask to have a strong filtering
		cp.add(sum(Cities)(i => element(distMatrix(i), succ(i))) == dist)
	} exploration {
		//exploration of the search tree
		while (!succ.forall(_.isBound)) {
			val res = minDomNotbound(succ)
			val (x, i) = res.first
			// get the closest successor in the domain of x
			val v = argMin((x.min to x.max).filter(x.hasValue(_)))(distMatrix(i)(_)).first
			cp.branch(cp.post(x == v))(cp.post(x != v))
		}
		
		visu.update
	}

	cp.printStats()
}
