package scampi.linprog.examples

import scampi.linprog.modeling._
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
object StochasticFarmer extends LPModel {
  def main(args: Array[String]) {

    val fileOut = new PrintWriter("stochasticFarmerSolution.csv")
    
    // Indexes
    val crops = Set("Wheat", "Corn", "Beet")
    val scenarios = Set("Bad", "Fair", "Good")
    val allVarGroups = Set("surface","buy","sell","cheapSell")
    val firstStage = Set("surface")
    val secondStage = allVarGroups diff firstStage

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
      val sol = solveDeterministicModel(s,None)

      // Evaluation on other sceanrios
      for {otherScenario <- scenarios; if otherScenario != s} {
        fileOut.write("Applying solution of scenario "+s+" to scenario "+otherScenario+"\n")
        solveDeterministicModel(otherScenario,Some(sol)) 
      }
    } 

    // Solve the stochastic model for all the scenarios
    solveStochasticModel()
    
    fileOut.close()
    
    /**
     * By providing a solution for the first stage variables you can will evaluate a first stage solution obtained by another mean. 
     */
    def solveDeterministicModel(scenario: String, firstStageSolution: Option[Solution]): Solution = {
      val detModel = new LPSolver(LPSolverLib.cplex)

      // Creating all variables 
      val detVars = Map[String, LPVar]()

      // First stage
      for { vG <- firstStage; crop <- crops } {
        firstStageSolution match {
          case Some(solution) =>
            val lb, ub = solution.getValue(vG + crop)
            detVars += (vG + crop -> new LPVar(detModel, vG + crop, lb, ub))
          case None =>
            detVars += (vG + crop -> new LPVar(detModel, vG + crop))
        }
      }

      // Second stage
      secondStage foreach { vG => crops foreach (crop => detVars += (vG + crop -> new LPVar(detModel, vG + crop))) }
      
      // Generating and solving the model
      detModel.minimize(sum(crops)(c => (detVars("surface" + c) * costs(c)) // Plantation costs
        + detVars("buy" + c) * buyPrices(c) // Purchases
        - detVars("sell" + c) * sellPrices(c) // Sales
        - detVars("cheapSell" + c) * cheapSellPrices(c))) subjectTo { // Over quota sales

        // Use all the surface
        detModel.add(sum(crops)(c => (detVars("surface" + c))) <= availableSurface)

        // Impose quotas
        crops.foreach(c => detModel.add(detVars("sell" + c) <= quotas(c)))

        // Production + purchases == consumption + sales
        crops.foreach(c => detModel.add(detVars("surface" + c) * stoYields(scenario)(c)
          + detVars("buy" + c)
          - detVars("sell" + c)
          - detVars("cheapSell" + c) >= requiredQties(c)))
      }
      
      // Display
      def displayDetSolution(variableGroup: String) {
        fileOut.write(crops.map(c => detVars(variableGroup + c).getValue).mkString(variableGroup + ";", ";", "\n"))
      }
      fileOut.write("Optimizing profit given yield factors of senario:" + scenario+"\n")
      fileOut.write("objective;" + (-1 * detModel.getObjectiveValue()) + "\n")
      allVarGroups foreach (vg => displayDetSolution(vg))
      
      new Solution(detVars filter ((t) => t._1 startsWith("surface")))
    }

    def solveStochasticModel(): Solution = {
      val stoModel = new LPSolver(LPSolverLib.cplex)
      val stoVars = Map[String, LPVar]()

      for (varGroup <- firstStage) {
        crops.foreach(crop => stoVars += (varGroup + crop -> new LPVar(stoModel, varGroup + crop)))
      }
      // All variables corresponding to second stage should be particularized to a scenario
      for (varGroup <- secondStage) {
        scenarios foreach (s => crops foreach (crop => stoVars += (varGroup + crop + s -> new LPVar(stoModel, varGroup + crop + s))))
      }

      stoModel.minimize((sum(crops)(c => (stoVars("surface" + c) * costs(c)))) // Plantation costs
        + (sum(scenarios, crops)((s, c) => (proba(s) * (
          stoVars("buy" + c + s) * buyPrices(c) // Purchases
          - stoVars("sell" + c + s) * sellPrices(c) // Sales
          - stoVars("cheapSell" + c + s) * cheapSellPrices(c)))))) subjectTo { // Over quota sales

        // Use all the surface
        stoModel.add(sum(crops)(c => (stoVars("surface" + c))) <= availableSurface)

        // Impose quotas
        scenarios foreach (s => crops.foreach(c => stoModel.add(stoVars("sell" + c + s) <= quotas(c))))

        // Production + purchases == consumption + sales
        scenarios foreach (s => crops.foreach(c => stoModel.add(stoVars("surface" + c) * stoYields(s)(c)
          + stoVars("buy" + c + s)
          - stoVars("sell" + c + s)
          - stoVars("cheapSell" + c + s) >= requiredQties(c))))
      }

      // Display
      fileOut.write("Optimizing EXPECTED profit given yield scenarios\n")
      fileOut.write("objective;" + (-1 * stoModel.getObjectiveValue()) + "\n")
      displayStoSolution("surface", None)
      for (s <- scenarios) {
        fileOut.write(s+"\n")
        secondStage foreach (g => displayStoSolution(g, Some(s)))
      }

      def displayStoSolution(variableGroup: String, scenario: Option[String]) {
        scenario match {
          case Some(s) =>
             fileOut.write(crops.map(c => stoVars(variableGroup + c + s).getValue).mkString(variableGroup + ";", ";", "\n"))
          case None =>
             fileOut.write(crops.map(c => stoVars(variableGroup + c).getValue).mkString(variableGroup + ";", ";", "\n"))
        }
      }
      
      new Solution(stoVars filter ((t) => t._1 startsWith("surface")))
    }
    
    /**
     * Store variable values
     */
    class Solution (vars: Map[String, LPVar]) {
    	val data = vars map (el => (el._1,el._2.getValue)) toMap
    	
    	def getValue(key :String) = data(key)
    	
    	override def toString():String = {data.mkString(";")} 
    }
  }
}