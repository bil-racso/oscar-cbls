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
import oscar.linprog._
import oscar.algebra._
import scala.collection.mutable.Map
import java.io.PrintWriter

/**
 * Farmer problem of "Introduction to stochastic programming" (Birge & Louveaux)
 *
 * Several types of crops to plant on a given land.
 * Crops are characterized by
 *  - *uncertain* yield
 *  - plantation cost
 *  - sell price
 *  - sell price over quotas
 *  - quantity required for own needs
 *
 * --> Maximize expected profit
 *
 *  @author bcr bcr@n-side.com
 */
object StochasticFarmer extends App {

  val fileOut = new PrintWriter("stochasticFarmerSolution.csv")

  // Indexes
  val crops = List("Wheat", "Corn", "Beet")
  val scenarios = List("Bad", "Fair", "Good")
  val allVarGroups = List("surface", "buy", "sell", "cheapSell")
  val firstStage = List("surface")
  val secondStage = allVarGroups.toSet diff firstStage.toSet toList

  // Data
  val costs = crops zip List(150.0, 230.0, 260.0) toMap // Cost by square meter
  val sellPrices = crops zip List(170.0, 150.0, 36.0) toMap // Price obtained for each Ton sold
  val cheapSellPrices = crops zip List(0.0, 0.0, 10.0) toMap // Price obtained for each Ton sold over quota
  val buyPrices = crops zip List(238.0, 210.0, 1000.0) toMap // Price paid for each Ton bought
  val requiredQties = crops zip List(200.0, 240.0, 0.0) toMap // Qty required for own needs (absolute)
  val quotas = crops zip List(Double.PositiveInfinity, Double.PositiveInfinity, 6000.0) toMap // Imposed quotas influencing prices
  val availableSurface = 500 // Total available surface
  val proba = scenarios zip List[Double](0.33, 0.33, 0.34) toMap // Probability of each scenario
  val detYields = crops zip List(2.5, 3.0, 20.0) toMap // Qty produced by square meter
  val yieldFactors = scenarios zip List(0.8, 1.0, 1.2) toMap // Influence of scenario on yields
  // Qty produced by square meter by scenario
  val stoYields = yieldFactors map ((s) => (s._1, detYields map ((t) => (t._1, s._2 * t._2))))

  // Deterministic optimization and evaluation on other scenarios
  for (s <- scenarios) {
    // Solve the deterministic model for each scenario
    val sol = solveDeterministicModel(s, None)

    // Evaluation on other sceanrios
    for { otherScenario <- scenarios; if otherScenario != s } {
      fileOut.write("Applying solution of scenario " + s + " to scenario " + otherScenario + "\n")
      solveDeterministicModel(otherScenario, Some(sol))
    }
  }

  // Solve the stochastic model for all the scenarios
  solveStochasticModel()

  fileOut.close()

  /**
   * By providing a solution for the first stage variables you can will evaluate a first stage solution obtained by another mean.
   */
  def solveDeterministicModel(scenario: String, firstStageSolution: Option[Solution]): Solution = {
    implicit val detModel = LPSolverLPSolve()

    // Creating all variables 
    val detVars = Map[String, LPFloatVar]()

    // First stage
    for { vG <- firstStage; crop <- crops } {
      firstStageSolution match {
        case Some(solution) =>
          val lb, ub = solution.getValue(vG + crop)
          detVars += (vG + crop -> LPFloatVar(vG + crop, lb, ub))
        case None =>
          detVars += (vG + crop -> LPFloatVar(vG + crop))
      }
    }

    // Second stage
    secondStage foreach { vG => crops foreach (crop => detVars += (vG + crop -> LPFloatVar(detModel, vG + crop))) }

    // Generating and solving the model
    minimize(sum(crops)(c => (detVars("surface" + c) * costs(c)) // Plantation costs
      + detVars("buy" + c) * buyPrices(c) // Purchases
      - detVars("sell" + c) * sellPrices(c) // Sales
      - detVars("cheapSell" + c) * cheapSellPrices(c)))

    // Over quota sales

    // Use all the surface
    add(sum(crops)(c => (detVars("surface" + c))) <= availableSurface)

    // Impose quotas
    for (c <- crops; if (quotas(c) != Double.PositiveInfinity)) {
      add(detVars("sell" + c) <= quotas(c))
    }
    // Production + purchases == consumption + sales
    crops.foreach(c => add(detVars("surface" + c) * stoYields(scenario)(c)
      + detVars("buy" + c)
      - detVars("sell" + c)
      - detVars("cheapSell" + c) >= requiredQties(c)))
    start()

    // Display
    def displayDetSolution(variableGroup: String) {
      fileOut.write(crops.map(c => detVars(variableGroup + c).value.get).mkString(variableGroup + ";", ";", "\n"))
    }
    fileOut.write("Optimizing profit given yield factors of senario:" + scenario + "\n")
    fileOut.write("objective;" + (-1 * detModel.objectiveValue.get) + "\n")
    allVarGroups foreach (vg => displayDetSolution(vg))

    new Solution(detVars filter ((t) => t._1 startsWith ("surface")))
  }

  def solveStochasticModel(): Solution = {
    implicit val stoModel = LPSolverLPSolve()
    val stoVars = Map[String, LPFloatVar]()

    for (varGroup <- firstStage) {
      crops.foreach(crop => stoVars += (varGroup + crop -> LPFloatVar(varGroup + crop)))
    }
    // All variables corresponding to second stage should be particularized to a scenario
    for (varGroup <- secondStage) {
      scenarios foreach (s => crops foreach (crop => stoVars += (varGroup + crop + s -> LPFloatVar(varGroup + crop + s))))
    }

    minimize((sum(crops)(c => (stoVars("surface" + c) * costs(c)))) // Plantation costs
      + (sum(scenarios, crops)((s, c) => (proba(s) * (
        stoVars("buy" + c + s) * buyPrices(c) // Purchases
        - stoVars("sell" + c + s) * sellPrices(c) // Sales
        - stoVars("cheapSell" + c + s) * cheapSellPrices(c))))))

    // Over quota sales

    // Use all the surface
    add(sum(crops)(c => (stoVars("surface" + c))) <= availableSurface)

    // Impose quotas
    for (s <- scenarios; c <- crops; if (quotas(c) != Double.PositiveInfinity)) {
      add(stoVars("sell" + c + s) <= quotas(c))
    }

    // Production + purchases == consumption + sales
    scenarios foreach (s => crops.foreach(c => add(stoVars("surface" + c) * stoYields(s)(c)
      + stoVars("buy" + c + s)
      - stoVars("sell" + c + s)
      - stoVars("cheapSell" + c + s) >= requiredQties(c))))
    start()

    // Display
    fileOut.write("Optimizing EXPECTED profit given yield scenarios\n")
    fileOut.write("objective;" + (-1 * objectiveValue.get) + "\n")
    displayStoSolution("surface", None)
    for (s <- scenarios) {
      fileOut.write(s + "\n")
      secondStage foreach (g => displayStoSolution(g, Some(s)))
    }

    def displayStoSolution(variableGroup: String, scenario: Option[String]) {
      scenario match {
        case Some(s) =>
          fileOut.write(crops.map(c => stoVars(variableGroup + c + s).value.get).mkString(variableGroup + ";", ";", "\n"))
        case None =>
          fileOut.write(crops.map(c => stoVars(variableGroup + c).value.get).mkString(variableGroup + ";", ";", "\n"))
      }
    }

    new Solution(stoVars filter ((t) => t._1 startsWith ("surface")))
  }

  /**
   * CPStore variable values
   */
  class Solution(vars: Map[String, LPFloatVar]) {
    val data = vars map (el => (el._1, el._2.value.get)) toMap

    def getValue(key: String) = data(key)

    override def toString(): String = { data.mkString(";") }
  }

}
