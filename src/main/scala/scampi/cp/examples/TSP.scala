/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.examples


import scampi.cp.modeling._
import scampi.cp.search._
import scampi.search.Branching

import scala.collection.JavaConversions._
import scala.io.Source
import java.lang._


/**
 * 
 * tsp model: given a distance matrix between 20 cities, 
 *            find the shortest tour visiting each city exactly once.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object TSP extends CPModel {

  def main(args: Array[String]) {

    val n = 20
    val Cities = 0 until n
    val lines = Source.fromFile("data/tsp.txt").getLines.toList

    val distMatrix = lines.grouped(n).map(i => i.map(j => j.toInt).toArray).toArray

    val cp = new CPSolver()
    //array of successors
    val succ = Array.tabulate(n)(_ => CPVarInt(cp, 0 until n))
    //total distance
    val dist = CPVarInt(cp, 0 to distMatrix.flatten.sum)

    cp.minimize(dist) subjectTo {
      cp.add(circuit(succ), Strong) //ask to have a strong filtering
      cp.add(sum(Cities)(i => element(distMatrix(i), succ(i))) == dist)
    } exploration {
      //exploration of the search tree
      while (!allBounds(succ)) {
         val res = minDomNotbound(succ)
         val (x, i) = res.first
         // get the closest successor in the domain of x
         val v = argMin((x.getMin() to x.getMax()).filter(x.hasValue(_)))(distMatrix(i)(_)).first
         cp.branch(cp.post(x == v)) (cp.post(x != v))
      }
    }

    cp.printStats()
  }

}