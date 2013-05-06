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

package oscar.examples.cp.multiobjective

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.visual.VisualFrame
import oscar.visual.PlotPareto
import java.awt.Color
import oscar.util.selectMin
import oscar.util.time
import oscar.util.reader.KnapsackReader._
import oscar.cp.multiobjective.SolSelect._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object BiKnapsack extends App {

  val dataFile = "data/mo-knapsack/2KP100A.txt"
  val solutionFile = "data/mo-knapsack/solution/2KP100A.txt"

  // Data
  // ----
  val (nItems: Int, capa1: Int, capa2: Int, items1: Array[(Int, Int)], items2: Array[(Int, Int)]) = read(dataFile)

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
  val sol = readSolution(solutionFile)
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

  val t = time { 
    cp.run() 
  }

  println("time " + t)
  println("size " + cp.nonDominatedSolutions.size)
}
