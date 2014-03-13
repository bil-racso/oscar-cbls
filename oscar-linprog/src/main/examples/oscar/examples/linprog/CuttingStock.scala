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

/**
 * @author Pierre Schaus pschaus@gmail.com
 * Cutting Stock using Column Generation
 */
object CuttingStock extends App {

  implicit val lp = LPSolver() 
    
  class Column(val x: LPFloatVar, val pattern: Array[Int]) {
    override def toString(): String = {
      pattern.mkString("\t")
    }
    def number(): Int = Math.ceil(x.value.get).toInt
  }

  val rollStock = 110
  val roll = Array(20, 45, 50, 55, 75)
  val demand = Array(48, 35, 24, 10, 8)
  val Rolls = 0 until roll.size


  var C: Array[Column] = Array()
  for (r <- Rolls) {
    val config = Array.tabulate(roll.size)(_ => 0)
    config(r) = rollStock / roll(r)
    C = C :+ new Column(LPFloatVar( "pattern" + r), config)
  }


  
  var constraints = Array[LPConstraint]()

  // Master Problem 
  minimize(sum(C)(c => c.x))

  for (r <- Rolls) {
    constraints = constraints :+ add(sum(C)(c => c.x * c.pattern(r)) >= demand(r))
  }

  start()
  println("master obj:" + objectiveValue)

  // Pricing Problem
  var objective = Double.MinValue
  do {
    
    implicit val mip = MIPSolver()
    val newPattern = Array.tabulate(roll.size)(_ => MIPIntVar(mip, "use", 0 to rollStock))
    val cost = Array.tabulate(roll.size)(constraints(_).dual)
    mip.add(sum(Rolls)(r => roll(r) * newPattern(r)) <= rollStock)
    mip.minimize(1 - sum(Rolls)(r => cost(r) * newPattern(r))) 
    mip.start()

    val x = lp.addColumn(1, constraints, newPattern.map(_.value.get)) //create a new variable by introducing a new column
    objective = mip.objectiveValue.get
    C = C :+ new Column(x, newPattern.map(_.value.get.toInt))
    println("master obj:" + lp.objectiveValue)
    
  } while (objective < 0)

  println("\n" + roll.mkString("\t"))
  println("-----------------------------------")
  C.foreach(c => println(c + " * " + c.number))
  println("-----------------------------------")
  println("total #boards:" + C.map(_.number).sum)

}
