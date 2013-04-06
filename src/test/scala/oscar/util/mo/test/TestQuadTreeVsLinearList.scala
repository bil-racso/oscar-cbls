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

package oscar.util.mo.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.util.mo._
import oscar.util.tree.Node
import oscar.visual.VisualFrame
import oscar.visual.VisualLabelledTree


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestQuadTreeVsLinearList extends FunSuite with ShouldMatchers {

  def printResult(quadTreeTime: Long, linearListTime: Long) = { 
    if(quadTreeTime < linearListTime) {
      println("QuadTree was " + ("%.2f".format(linearListTime.toDouble/quadTreeTime)) + "x faster than linearList\n" +
          "(Quad: " + quadTreeTime + " nanoSeconds, List: " +
          + linearListTime + " nanoSeconds)")
    }
    else {
      println("LinearList was " + ("%.2f".format(quadTreeTime.toDouble/linearListTime)) + "x faster than QuadTree\n" +
          "(List: " + linearListTime + " nanoSeconds, Quad: " +
          + quadTreeTime + " nanoSeconds)")
    }
  }
  
  val randGen = new scala.util.Random(42)
/*
 * INFO:
 * http://www.r-bloggers.com/ascii-scatterplots-in-r/
 * source("http://biostatmatt.com/R/scat.R")
 * data(co2) #Mauna Loa Atmospheric CO2 Concentration
 * scat(c(co2[1:75]), rows=10, cols=80)
 */
  
// ______________________________________________________
//|                                                      |
//|  * x1=(10,50)                          x6=(50,50) *  |
//|                                                      |
//|                                                      |
//|                                 x5=(45,45) *         |
//|                                                      |
//|                          * x2=(30,40)                |
//|                                                      |
//|                                                      |
//|              * x8=(20,35)                            |
//|                                                      |
//|  * x7=(10,30)                        * x3=(40,30)    |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                        x4=(50,10) *  |
//|______________________________________________________|
  
	val x1 = MOOPoint(Array(10,50), Array(10,50))
	val x2 = MOOPoint(Array(30,40), Array(30,40))
	val x3 = MOOPoint(Array(40,30), Array(40,30)) 
	val x4 = MOOPoint(Array(50,10), Array(50,10))
	val x5 = MOOPoint(Array(45,45), Array(45,45))
	val x6 = MOOPoint(Array(50,50), Array(50,50))
	val x7 = MOOPoint(Array(10,30), Array(10,30))
	val x8 = MOOPoint(Array(20,35), Array(20,35))
	
    /* Hypercubes with n random points, in n dimensions and edge size of k */
	def hyperFront(nbPoints: Int, nbDimensions: Int, edgeSize: Double): Array[MOOPoint[Array[Double], Double]] = {
	  Array.tabulate(nbPoints)(i => MOOPoint(Array.tabulate(nbDimensions)(i => randGen.nextDouble * edgeSize), Array.tabulate(nbDimensions)(i => scala.util.Random.nextDouble * edgeSize)))
	}
    
    test("Test QuadTree vs LinearList 1 - Dummy 2D") {
	  val treeBegin = System.nanoTime()
	  val tree = QuadTree(x2)
	  tree.insert(x4)
	  tree.insert(x3)
	  tree.insert(x8)
	  tree.insert(x1)
	  val quadTreeTime = System.nanoTime() - treeBegin
	  
	  val listBegin = System.nanoTime()
	  val list = LinearList(x2)
	  list.insert(x4)
	  list.insert(x3)
	  list.insert(x8)
	  list.insert(x1)
	  val linearListTime = System.nanoTime() - listBegin
	  
	  tree.toSet should be(list.toSet)
	  printResult(quadTreeTime, linearListTime)
	}
	
	test("Test QuadTree vs LinearList 2 - Dummy 2D") {
	  val treeBegin = System.nanoTime()
	  val tree = QuadTree(x2)
	  tree.insert(x4)
	  tree.insert(x3)
	  tree.insert(x8)
	  tree.insert(x1)
	  val quadTreeTime = System.nanoTime() - treeBegin
	  
	  val listBegin = System.nanoTime()
	  val list = LinearList(x2)
	  list.insert(x4)
	  list.insert(x3)
	  list.insert(x8)
	  list.insert(x1)
	  val linearListTime = System.nanoTime() - listBegin
	  
	  tree.toSet should be(list.toSet)
	  printResult(quadTreeTime, linearListTime)
	}
	
	test("Test QuadTree vs LinearList 3 - Hypercube 2D") {
	  
	  val front2D = hyperFront(1000, 2, 1)
	  
	  val treeBegin = System.nanoTime()
	  val tree = new QuadTree(front2D(0))
	  for (i <- 1 until front2D.length) {
	    front2D(i).label = i.toString
	    tree.insert(front2D(i))
	  }
	  val quadTreeTime = System.nanoTime() - treeBegin
	  
	  val listBegin = System.nanoTime()
	  val list = new LinearList(front2D(0))
	  for (i <- 1 until front2D.length)
	    list.insert(front2D(i))
	  val linearListTime = System.nanoTime() - listBegin
	  
	  tree.toSet should be(list.toSet)
	  printResult(quadTreeTime, linearListTime)
	}
	
	test("Test QuadTree vs LinearList 4 - Hypercube 3D") {
	  
	  val front3D = hyperFront(1000, 3, 1)
	  
	  val treeBegin = System.nanoTime()
	  val tree = new QuadTree(front3D(0))
	  
	  for (i <- 1 until front3D.length) {
	    tree.insert(front3D(i))
	  }
	  val quadTreeTime = System.nanoTime() - treeBegin
	  
	  val listBegin = System.nanoTime()
	  val list = new LinearList(front3D(0))
	  for (i <- 1 until front3D.length)
	    list.insert(front3D(i))
	  val linearListTime = System.nanoTime() - listBegin
	  
	  printResult(quadTreeTime, linearListTime)
	}
}