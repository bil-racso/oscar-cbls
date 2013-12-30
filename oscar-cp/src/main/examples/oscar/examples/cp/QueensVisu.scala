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
import oscar.cp.core._
import oscar.util._
import oscar.visual.VisualFrame
import oscar.algo.search.VisualSearchTree
import oscar.util.tree.Tree
import oscar.visual.VisualGrid
import java.awt.Color

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensVisu extends App {

  val cp = CPSolver()

  val n = 8 //number of queens
  val Queens = 0 until n
  //variables
  val queens = for (i <- Queens) yield CPVarInt(1 to n)(cp)

  val tree = new Tree() 
  // -----------------------------------------------
  val f = new VisualFrame("ParcelShipment", 1, 2)
  val w1 = f.createFrame("Tree")
  val vt = new VisualSearchTree(tree)
  w1.add(vt)
  w1.pack()

  val w2 = f.createFrame("Queens")
  val vg = new VisualGrid(n, n)
  w2.add(vg)
  w2.pack()

  def updateVisu(doms: Seq[Set[Int]]) {
    for (q <- Queens; qv <- Queens) {
      val col = if (doms(q).contains(qv)) Color.green else Color.red
      vg(q)(qv).innerCol = col
    }
  }
  // -------------------------------------------------
  var currNode = 0
  var nbsol = 0
  
  cp.onSolution { 
    nbsol += 1 
    tree.addSuccess(currNode)
  }



  cp.solve subjectTo {
    cp.add(allDifferent(queens) /*,Strong*/ )
    cp.add(allDifferent(for (i <- Queens) yield queens(i) + i) /*,Strong*/ )
    cp.add(allDifferent(for (i <- Queens) yield queens(i) - i) /*,Strong*/ )
  } search {
    select(queens)(x => !x.isBound) match {
      case None => noAlternative
      case Some(x) => {
            val parent = currNode
            val v = x.min
            branch {
              cp.add(x == v)
              currNode += 1
              val doms = queens.map(_.toSet)
              tree.createBranch(parent,currNode,currNode.toString,"left") {
            	  updateVisu(doms)
              }
            } {
              cp.add(x != v)
              currNode += 1
              val doms = queens.map(_.toSet)
              tree.createBranch(parent,currNode,currNode.toString,"right") {
                updateVisu(doms)
              }              
            }        
      }
    }

  }
  val stats = cp.start(3)
  vt.update()
  //print some statistics
  println("#sol:" + nbsol)
  println(stats)

}
