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


package oscar.examples.cp

import oscar.cp.modeling._
import oscar.algo.search._
import oscar.cp.core._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._
import java.awt.Color
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualCircle

/**
 * P-Median Problem
 * 
 * Let us consider a set I={1,..., n} of potential locations for p facilities, 
 * a set J={1,..., m} of customers, and  n x m matrix of transportations costs 
 * for satisfying the demands of the customers from the facilities.  
 * The p-median problem is to locate the p facilities at locations of I in order 
 * to minimize the total transportation cost for satisfying the demand of the customers. 
 * Also each location has fixed capacity for the demand that cannot be exceeded.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object PMedian extends App {

  val lines = Source.fromFile("data/pmed.txt").getLines.reduceLeft(_ + " " + _)

  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }

  val nbCust = next()
  val nbMed = next()
  val capa = next()
  val cust =
    for (i <- 0 until nbCust) yield {
      next()
      (next(), next(), next())
    }
  val dist = Array.tabulate(nbCust, nbCust) { (i, j) =>
    val xdist = (cust(i)._1 - cust(j)._1)
    val ydist = (cust(i)._2 - cust(j)._2)
    Math.sqrt(xdist * xdist + ydist * ydist).toInt
  }
  val demand = for (i <- 0 until nbCust) yield cust(i)._3

  val cost = Array.tabulate(nbCust, nbCust)((i, j) => dist(i)(j))

  val cp = CPSolver()
  val x = Array.fill(nbCust)(CPVarInt(cp, 0 until nbCust))
  val xsol = Array.fill(nbCust)(0)
  val load = Array.fill(nbCust)(CPVarInt(cp, 0 until capa))

  // ----------- visu ----------
  val f = new VisualFrame("P-Median Problem",1,2)
  val w = f.createFrame("Layout")
  val scale = 5
  val offsetx = 100
  val offsety = 100
  val drawing = VisualDrawing(true)
  w.add(drawing)
  val vcircles = for (i <- 0 until nbCust) yield {
    new VisualCircle(drawing, cust(i)._1 * scale + offsetx, cust(i)._2 * scale  + offsety, demand(i))
  }
  val vlines = for (i <- 0 until nbCust) yield {
    VisualLine(drawing, cust(i)._1 * scale + offsetx, cust(i)._2 * scale + offsety, 0, 0)
  }
  f.pack()

  def updateVisu() { 
    for (i <- 0 until nbCust) {
      val j = x(i).value
      vcircles(i).innerCol = Color.WHITE;
      vcircles(j).innerCol = Color.RED
      vlines(i).dest = (cust(j)._1 * scale + offsetx, cust(j)._2 * scale  + offsety)
    }
  }
  // ---------------------------

  val rnd = new scala.util.Random(0)
  
  
  
  val costs = Array.tabulate(nbCust)(i => cost(i)(x(i)))
  val totCost = sum(costs) 

  cp.minimize(totCost) subjectTo {
    cp.add(binPacking(x, demand, load))
    cp.add(sum(0 until nbCust)(i => load(i) >>= 0) <= nbMed)
  } exploration {
    cp.binaryFirstFail(x,_.randomValue)
    for (i <- 0 until nbCust) xsol(i) = x(i).value
    updateVisu()
    println("\n"+totCost)
  } run()

}
