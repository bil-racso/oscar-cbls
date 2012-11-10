package oscar.cbls.routing.visual

import oscar.cbls.invariants.core.computation.{IntVar, Model}
import oscar.cbls.routing._
import heuristic.{Unrouted, NearestNeighbor, RandomNeighbor}
import oscar.visual.{VisualDrawing, VisualArrow}
import oscar.cbls.constraints.core.ConstraintSystem
import math._
import oscar.cbls.constraints.lib.basic.{EQ, GE, LE}
import oscar.cbls.search.StopWatch

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 8/11/12
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
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

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/


object ModelVRP {
  val model = new ModelVRP()
}

class ModelVRP() extends StopWatch{

  // model's problem definition
  var it =0
  var V:Int = 0 // nb of vehicles
  var N:Int = 0 // random nb of towns
  var closeNeighbor:Int =  0// save close neighbors
  var m: Model = null
  var vrp: VRP with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
    /*with SymmetricVRP*/ with Predecessors with PenaltyForUnrouted with OtherFunctionToObjective with Constraints
    with WeightedNode = null
  var towns:Array[Town] = null
  var distMatrix : Array[Array[Int]]= null
  var arrows :Array[VisualArrow]=  null
  var strongConstraintSystem:ConstraintSystem = null
  var weakConstraintSystem:ConstraintSystem = null
  var strongPenalty:IntVar = null
  var weakPenalty:IntVar = null


  // helpers methods

  /*
   Get the actual route in a String format.
  */
  def getRoute(vrp:VRP):String={
    var route = ""
    route += towns(0).name
    var next = vrp.Next(0).getValue(true)
    while(next != 0){
      route += " -> "+towns(next).name
      next = vrp.Next(next).getValue(true)
    }
    route
  }

  //clone the Next array in case of new test on the same instance
  var clonedNext:Array[Int] = null
  def cloneHeuristic {
    clonedNext = new Array[Int](N)
    for(i <- 0 until N)
      clonedNext(i) = vrp.Next(i).value
  }

  def getInstance(boardPanel:Dashboard,mapPanel:VisualDrawing):Array[Town] = {
    boardPanel.instances.getSelectedIndex match{
      case n => {InstanceVisualVRP.random(N,(mapPanel.getSize().getWidth).toInt-100,(mapPanel.getSize().getHeight).toInt-100,n)}
    }
  }

  def getDistMatrix(towns : Array[Town]):Array[Array[Int]] = {
    Array.tabulate(N,N)((i,j) => round(sqrt((pow(towns(i).long - towns(j).long, 2)
      + pow(towns(i).lat - towns(j).lat, 2) ).toFloat)).toInt )
  }




  def initModel(panelVRP:PanelVRP,reset:Boolean=false):ModelVRP ={
    startWatch()
    val boardPanel = panelVRP.boardPanel
    val mapPanel = panelVRP.mapPanel
    V = boardPanel.nbVehicle.getText.toInt
    N = boardPanel.nbNodes.getText.toInt

    closeNeighbor = boardPanel.klimited.getText.toInt
    m = new Model(false,false,false,false)
    vrp = new VRP(N, V, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr
      with ClosestNeighborPoints /*with SymmetricVRP*/ with Predecessors with PenaltyForUnrouted with OtherFunctionToObjective
      with WeightedNode with Constraints

    // constraints definition
    strongConstraintSystem = new ConstraintSystem(m)
    weakConstraintSystem = new ConstraintSystem(m)
    val maxNodes = boardPanel.strongCField.getText.toInt
    val minNodes = boardPanel.weakCField.getText.toInt
    strongPenalty = new IntVar(m,Int.MinValue,Int.MaxValue,boardPanel.penaltySCField.getText.toInt,"StrongC. penality")
    weakPenalty = new IntVar(m,Int.MinValue,Int.MaxValue,boardPanel.penaltyWCField.getText.toInt,"WeakC. penality")
    // Example of constraint (with ConstraintSystem) on length of route.
    // It's also easy to fix a different penalty according to route number.
    for(i <- 0 until V){
      strongConstraintSystem.post(LE(vrp.RouteLength(i),new IntVar(m,0,N,maxNodes,"max node in route "+i)),strongPenalty)
      strongConstraintSystem.registerForViolation(vrp.RouteLength(i))
    }
    for(i <- 0 until V){
      weakConstraintSystem.post(EQ(vrp.RouteLength(i),new IntVar(m,0,N,minNodes,"max node in route "+i)),weakPenalty)
      weakConstraintSystem.registerForViolation(vrp.RouteLength(i))
    }

    // Example of constraint on no routed nodes (penalty of 1000 for each unrouted node).
    /*
    val penaltyUnrouted = 1000
    vrp.fixUnroutedPenaltyWeight(penaltyUnrouted)
    // easy to fix another penalty on given nodes.
    //val givenNode =
    //vrp.fixUnroutedPenaltyWeight(2*penaltyUnrouted,givenNode)
    vrp.recordAddedFunction(vrp.UnroutedPenalty)
    */

    if(boardPanel.strongCButton.isSelected)
      vrp.setStrongConstraints(strongConstraintSystem)
    if (boardPanel.weakCButton.isSelected)
      vrp.setWeakConstraints(weakConstraintSystem)

    towns = getInstance(boardPanel,mapPanel)

    distMatrix = getDistMatrix(towns)
    vrp.installCostMatrix(distMatrix)
    vrp.saveKNearestPoints(closeNeighbor)

    strongConstraintSystem.close()
    weakConstraintSystem.close()
    m.close()
    println("Model closed " + getWatchString)
    if (reset){
      for (i <- 0 until clonedNext.length)
        vrp.Next(i) := clonedNext(i)
    }
    else{
      boardPanel.heuristic.getSelectedIndex match {
        case 0 => RandomNeighbor(vrp)
        case 1 => NearestNeighbor(vrp)
        case 2 => Unrouted(vrp)
      }
    }
    m.propagate()
    cloneHeuristic
    it = 0
    return this
  }



}