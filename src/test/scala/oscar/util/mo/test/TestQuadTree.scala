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
import oscar.util.mo.MOOPoint
import oscar.util.mo.QuadTree
import scala.collection.immutable.LinearSeq
import oscar.visual.VisualFrame
import oscar.visual.VisualLabelledTree
import oscar.util.tree.Node


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestQuadTree extends FunSuite with ShouldMatchers {

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
	
	val A = MOOPoint(Array(42), Array(1, 1, 1, 1, 1))
	val B = MOOPoint(Array(42), Array(0, 2, 1, 4, -5))
	
	test("Test QuadTree 1") {
		val tree = new QuadTree(x1)
		tree.insert(x2)
		tree.insert(x3)
		tree.size should be(3)
		tree.toSet should be(Set(x1,x2,x3))
	}
	
	test("Test QuadTree 2") {
		val tree = new QuadTree(x4)
		tree.insert(x2)
		tree.insert(x3)
		tree.insert(x1)
		tree.insert(x3)
		tree.insert(x4)
		tree.insert(x1)
		tree.size should be(4)
		tree.toSet should be(Set(x1,x2,x3,x4))
	}
	
	test("Test QuadTree 3") {
		val tree = new QuadTree(x1)
		tree.insert(x5)
		tree.insert(x3)
		tree.insert(x2)
		tree.size should be(2)
		tree.toSet should be(Set(x1,x5))
	}
	
	test("Test QuadTree 4") {
		val tree = new QuadTree(x1)
		tree.insert(x5)
		tree.insert(x3)
		tree.insert(x2)
		tree.insert(x1)
		tree.insert(x5)
		tree.insert(x7)
		tree.size should be(2)
		tree.toSet should be(Set(x1,x5))
	}
	
	test("Test QuadTree 5") {
		val tree = new QuadTree(x1)
		tree.insert(x5)
		tree.insert(x3)
		tree.insert(x2)
		tree.insert(x1)
		tree.insert(x5)
		tree.insert(x7)
		tree.insert(x8)
		tree.insert(x6)
		tree.size should be(1)
		tree.toSet should be(Set(x6))
	}
	
	test("Test QuadTree 6") {
		val tree = new QuadTree(x4)
		tree.insert(x3)
		tree.insert(x8)
		tree.insert(x1)
		tree.size should be(4)
		tree.toSet should be(Set(x4,x3,x8,x1))
		
		tree.insert(x7)
		tree.size should be(4)
		tree.toSet should be(Set(x4,x3,x8,x1))
		
		tree.insert(x2)
		tree.size should be(4)
		tree.toSet should be(Set(x4,x3,x2,x1))
		
		tree.insert(x5)
		tree.size should be(3)
		tree.toSet should be(Set(x4,x5,x1))		
	}
	
    test("Test QuadTree 7") {
		val tree = new QuadTree(x3)
		tree.insert(x4)
		tree.insert(x7)
		tree.insert(x8)
		tree.size should be(3)
		tree.toSet should be(Set(x3,x4,x8))
	}
    
    test("Test QuadTree 8") {
		val tree = QuadTree(x2)
		tree.insert(x4)
		tree.insert(x3)
		tree.insert(x8)
		tree.insert(x1)
		tree.size should be(4)
		tree.toSet should be(Set(x4,x3,x2,x1))
		
		tree.insert(x7)
		tree.size should be(4)
		tree.toSet should be(Set(x4,x3,x2,x1))
		
		tree.insert(x2)
		tree.size should be(4)
		tree.toSet should be(Set(x4,x3,x2,x1))
		
		tree.insert(x5)
		tree.size should be(3)
		tree.toSet should be(Set(x4,x5,x1))		
	}
    
    test("Test QuadTree 9") {
      val A = MOOPoint(Array(42, 42), Array(4, 1))
      val B = MOOPoint(Array(42, 42), Array(1, 5))
      val C = MOOPoint(Array(42, 42), Array(2, 4))
      val D = MOOPoint(Array(42, 42), Array(3, 3))
      val E = MOOPoint(Array(42, 42), Array(5, 2))
      val tree = new QuadTree(A)
      tree.insert(B)
      tree.insert(C)
      tree.insert(D)
      tree.insert(E)
      tree.size should be(4)
      tree.toSet should be(Set(B,C,D,E))
    }
    
    test("Test QuadTree 10 - Dummy 3D") {
      val A0 = MOOPoint(Array(42, 42), Array(1, 4, 3))
      val A1 = MOOPoint(Array(42, 42), Array(5, 6, 0))
      val A2 = MOOPoint(Array(42, 42), Array(6, 3, 2))
      val A3 = MOOPoint(Array(42, 42), Array(3, 5, 1))
      val A4 = MOOPoint(Array(42, 42), Array(4, 2, 5))
      val A5 = MOOPoint(Array(42, 42), Array(0, 0, 4))
      val A6 = MOOPoint(Array(42, 42), Array(2, 1, 6))
      
      val tree = new QuadTree(A0)
      
      tree.insert(A1)
      tree.insert(A2)
      tree.insert(A3)
      tree.insert(A4)
      println("\n" * 5)
      tree.insert(A5)
      tree.insert(A6)
      println(tree.toString)
      tree.size should be(6)
      tree.toSet should be(Set(A0, A1, A2, A3, A4, A6))
    }
    
    test("Test QuadTree 11 - Replace 2D") {
      val A = MOOPoint(Array(42, 42), Array(100, 100))
      val B = MOOPoint(Array(42, 42), Array(95, 105))
      val C = MOOPoint(Array(42, 42), Array(110, 90))
      val D = MOOPoint(Array(42, 42), Array(90, 110))
      val E = MOOPoint(Array(42, 42), Array(105, 95))
      val F = MOOPoint(Array(42, 42), Array(115, 85))
      val G = MOOPoint(Array(42, 42), Array(101, 101))
      
      val tree = new QuadTree(A)
      tree.insert(B)
      tree.insert(C)
      tree.insert(D)
      tree.insert(E)
      tree.insert(F)
      tree.insert(G)

      tree.size should be(6)
      tree.toSet should be(Set(B, C, D, E, F, G))
    }
    
    test("Test QuadTree 12 - Replace and Delete 3D") {
      val A = MOOPoint(Array(42, 42), Array(100, 100, 100))
      A.label = "A"
      val B = MOOPoint(Array(42, 42), Array(95, 95, 105))
      B.label = "B"
      val C = MOOPoint(Array(42, 42), Array(95, 105, 95))
      C.label = "C"
      val D = MOOPoint(Array(42, 42), Array(105, 95, 95))
      D.label = "D"
      val E = MOOPoint(Array(42, 42), Array(102, 97, 93))
      E.label = "E"
      val F = MOOPoint(Array(42, 42), Array(101, 96, 94))
      F.label = "F"
      val G = MOOPoint(Array(42, 42), Array(105, 96, 92))
      G.label = "G"
      val H = MOOPoint(Array(42, 42), Array(103, 103, 103))
      H.label = "H"
      
      val tree = new QuadTree(A)
      tree.insert(B)
      tree.insert(C)
      tree.insert(D)
      tree.insert(E)
      tree.insert(F)
      tree.insert(G)
      tree.insert(H)
      
      /*
      println(tree)
      val f = new VisualFrame("This is DFO!")
      val inf = f.createFrame("QuadTree")
      val PAUSE = 250000
	  val visualTree = new VisualLabelledTree(Node.design(tree.toNode, 155))
      inf.add(visualTree)
	  f.pack()
	  visualTree.repaint()
	  Thread.sleep(PAUSE);
	  */
      
      tree.size should be(5)
      tree.toSet should be(Set(B, C, D, H, G))
    }
}
