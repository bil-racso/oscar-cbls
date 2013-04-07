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

package oscar.cp.mem.knapsack;

import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source
import java.lang._
import oscar.visual.VisualFrame
import oscar.cp.mem.visu.PlotPareto
import scala.collection.JavaConversions._
import oscar.visual.PlotLine
import oscar.cp.mem.measures.Hypervolume
import java.awt.Color
import oscar.util.OutFile
import oscar.util.selectMin
import oscar.cp.mem.SolSelect._
import oscar.search.IDSSearchController

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
object MoKnapsack extends App {
  
  // Parameters
  // ----------
  val maxTime = 1000*60
  val intensProb = 50
  val rankingProb = 90
  val nRelax = 10
  val seed = 0

  val rand = new scala.util.Random(seed)
  
  val dataFile = "data/mo-knapsack/2KP100A.txt"
  val solutionFile = "data/mo-knapsack/solution/2KP100A.txt"

  // Data
  // ----
  val (nItems: Int, capa1: Int, capa2: Int, items1: Array[(Int, Int)], items2: Array[(Int, Int)]) = KnapsackReader.read(dataFile)

  val Items = 0 until nItems
  val nObjs = 2
  val Objs = 0 until nObjs

  val weight = Array(items1.map(_._1), items2.map(_._1))
  val profit = Array(items1.map(_._2), items2.map(_._2))
  val ratio = for (o <- Objs) yield (Items.map(i => profit(o)(i).toDouble / weight(o)(i)).toArray)

  // Visualization
  // -------------
  val f = new VisualFrame("Knapsack MO", 1, 2)
  val paretoPlot = new PlotPareto(nbPareto = 2, objMax1 = true, objMax2 = true)
  val sol = KnapsackReader.readSolution(solutionFile)
  for (i <- 0 until sol.size) {
    val (o1, o2) = sol(i)
    paretoPlot.insert(o1, o2, 1)
  }
  f.add(paretoPlot)
  f.pack()

  // Model
  // -----
  val cp = CPSolver()
  cp.silent = true

  // for each facilities, the location chosen for it
  val x: Array[CPVarBool] = Array.fill(nItems)(CPVarBool(cp))

  val capaVar1 = CPVarInt(cp, 0 to capa1)
  val capaVar2 = CPVarInt(cp, 0 to capa2)
  val profitVar1 = CPVarInt(cp, 0 to profit(0).sum)
  val profitVar2 = CPVarInt(cp, 0 to profit(1).sum)

  val knapsack1 = binaryKnapsack(x, items1.map(_._2), items1.map(_._1), profitVar1, capaVar1)
  val knapsack2 = binaryKnapsack(x, items2.map(_._2), items2.map(_._1), profitVar2, capaVar2)

  cp.addDecisionVariables(x)
  cp.addDecisionVariables(Array(capaVar1, capaVar2))

  cp.paretoMaximize(profitVar1, profitVar2) subjectTo {
    cp.add(knapsack1)
    cp.add(knapsack2)
  }

  var obj = 0
  cp.exploration {
    while (!allBounds(x)) {
      val i = selectMin(0 until x.size)(!x(_).isBound)(-ratio(obj)(_)).get
      cp.branch(cp.post(x(i) == 1))(cp.post(x(i) == 0))
    }
    paretoPlot.insert(profitVar1.value, profitVar2.value)
  }

  // First solution
  cp.run(1)

  var iter = 0
  val t0 = System.currentTimeMillis()

  while (System.currentTimeMillis() - t0 < maxTime) {  

    iter += 1
    
    // Solution selection
    val sol = selectSol()

    // Optimization mode
    val intens = rand.nextInt(100) < intensProb
    if (intens) cp.objective.intensify(sol)
    else cp.objective.diversify()
    
    // Visu
    paretoPlot.highlight(sol(profitVar1), sol(profitVar2), if (intens) Color.red else Color.green)

    for (o <- 0 to 1) {
      obj = o
      cp.runSubjectTo(failureLimit = 1000) {
        // Keeps items assigned to the solution
        val filtered = Items.filter(i => sol(x(i)) == 1)
        // Sorts them according to profit/weight
        val sorted = filtered.sortBy(ratio(obj)(_))

        val selected = Array.fill(nItems)(true)

        var ns = 0
        for (i <- sorted if ns < nRelax) {
          if (rand.nextInt(100) < rankingProb) {
            selected(i) = false
            ns += 1
          }
        }

        val toFix = sorted.filter(selected(_))
        cp.post(toFix.map(i => x(i) == sol(x(i))).toArray[Constraint])
      }
    }
  }

  def selectSol(): CPSol = {
    val sols = (for (x <- cp.nonDominatedSolutions) yield x).toArray
    //sampling2D(sols.map(s => (s, (s(profitVar1), s(profitVar2)))))._1
    sols(rand.nextInt(sols.size))
  }

  val time = System.currentTimeMillis() - t0
  println("time " + time)
  println("iter " + iter)
  println("size " + cp.nonDominatedSolutions.size)
}
