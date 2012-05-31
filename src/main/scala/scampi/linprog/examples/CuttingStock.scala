/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples

import scampi.linprog.modeling._

/**
 * @author Pierre Schaus pschaus@gmail.com
 * Cutting Stock using Column Generation
 */
object CuttingStock extends LPModel with MIPModel {
	
  class Column (val x : LPVar, val pattern : Array[Int]) {
	  override def toString() : String = {
	 	  pattern.mkString("\t")
	  }   
	  def number() : Int = Math.ceil(x.getValue).toInt
  }
  
  
  def main(args: Array[String]) {
	  	  
	  val rollStock = 110	  
	  val roll =  Array(20, 45, 50, 55, 75)
	  val demand = Array(48, 35, 24, 10,  8)
	  val Rolls = 0 until roll.size
	  
	  val lp = LPSolver(LPSolverLib.lp_solve)
	  var C : Array[Column] = Array()
	  for (r <- Rolls) {
	 	  val config = Array.tabulate(roll.size)(_ => 0)
	 	  config(r) = rollStock/roll(r)
	 	  C = C :+ new Column(LPVar(lp,"pattern"+r), config)
	  }
	   
	  var constraints = Array[LPConstraint]()
 
	  // Master Problem 
	  lp.minimize(sum(C)(c => c.x)) subjectTo {
	 	  for (r <- Rolls) {
	 	 	  constraints = constraints :+ lp.add(sum(C)(c => c.x * c.pattern(r)) >= demand(r))
	 	  }
	  }
	  println("master obj:" + lp.getObjectiveValue)
	  
	  // Pricing Problem
	  var mip : MIPSolver = null
	  do {
		  mip = MIPSolver()
		  val newPattern = Array.tabulate(roll.size)(_ => MIPVar(mip,"use",0 to rollStock))
		  val cost = Array.tabulate(roll.size)(constraints(_).getDual)

		  mip.minimize(1 - sum(Rolls)(r => cost(r) * newPattern(r))) subjectTo {
			  mip.add(sum(Rolls)(r => roll(r) * newPattern(r)) <= rollStock)
		  }

		  val x = lp.addColumn(1,constraints, newPattern.map(_.getValue)) //create a new variable by introducing a new column
		  
		  C = C :+ new Column(x, newPattern.map(_.getValue.toInt))		
		  
		  println("master obj:" + lp.getObjectiveValue)
		  

	  } while(mip.getObjectiveValue < 0)

	  	  
	  println("\n"+roll.mkString("\t"))
	  println("-----------------------------------")
	  C.foreach(c => println(c+" * "+c.number))
	  println("-----------------------------------")
	  println("total #boards:" + C.map(_.number).sum)
	  
  }
}
