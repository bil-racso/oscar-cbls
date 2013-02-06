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

package oscar.util.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.util._


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
  
	val x1 = Array(10,50)
	val x2 = Array(30,40)
	val x3 = Array(40,30)
	val x4 = Array(50,10)
	val x5 = Array(45,45)
	val x6 = Array(50,50)
	val x7 = Array(10,30)
	val x8 = Array(20,35)
  
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
		val tree = IntQuadTree(2)
		tree.insert(x4)
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
	

}
