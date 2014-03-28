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
package oscar.examples.linprog

import oscar.linprog.modeling._
import oscar.algebra._
import scala.io.Source

/**
 * @author Pierre Schaus pschaus@gmail.com
 * BinPacking using Column Generation
 */
object BinPacking extends App {
  
  var lines = Source.fromFile("data/bp/u100_00.bpa").getLines.toList.drop(1)
  val data = lines.head.split(" ").map(_.toInt)
  val weights = lines.drop(1).map(_.toInt)
  val n = weights.size
  println(weights)
  
  implicit val lp = LPSolver(LPSolverLib.glpk) 
    
  class Column(val x: LPFloatVar, val pattern: Array[Int]) {
    override def toString(): String = {
      pattern.mkString("\t")
    }
    def number(): Int = Math.ceil(x.value.get).toInt
  }

  val capa = 150

  var C: Array[Column] = Array()
  for (i <- 0 until n) {
    val config = Array.fill(n)(0)
    config(i) = 1
    C = C :+ new Column(LPFloatVar( "pattern" + i), config)
  }

  var constraints = Array[LPConstraint]()

  // Master Problem 
  minimize(sum(C)(c => c.x))
  
  for (i <- 0 until n) {
    // item i must be selected at least once
    constraints = constraints :+ add(sum(C)(c => c.x * c.pattern(i)) >= 1)
  }

  start()
  println("master obj:" + objectiveValue)
  
  // Pricing Problem
  var reducedCost = Double.MinValue
  do {
    
    implicit val mip = MIPSolver()
    val newPattern = Array.tabulate(n)(_ => MIPIntVar(mip, "use", 0 to 1))
    val cost = Array.tabulate(n)(constraints(_).dual)
    
    mip.add(sum(0 until n)(i => weights(i) * newPattern(i)) <= capa)
    mip.minimize(1 - sum(0 until n)(r => cost(r) * newPattern(r))) 
    mip.start()

    val x = lp.addColumn(1, constraints, newPattern.map(_.value.get)) //create a new variable by introducing a new column
    reducedCost = mip.objectiveValue.get
    println("pricing obj:"+reducedCost)
    C = C :+ new Column(x, newPattern.map(_.value.get.toInt))
    //objective = lp.objectiveValue.get
    
    println("master obj:" + lp.objectiveValue.get)
    println("reduce cost:" + (reducedCost))
    println("master objlb:" + (lp.objectiveValue.get+reducedCost*n))
    
    
  } while (reducedCost < 0)
    
  println("number of columns"+C.size)
  println(C.map(_.x.value.get).mkString(","))
  println("number of bins:"+C.map(_.number).sum)
/*
  println("\n" + roll.mkString("\t"))
  println("-----------------------------------")
  C.foreach(c => println(c + " * " + c.number))
  println("-----------------------------------")
  println("total #boards:" + C.map(_.number).sum)
*/
}
