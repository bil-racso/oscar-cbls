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

package oscar.cbls.routing.visual

import oscar.cbls.routing._
import initialSolution.RandomNeighbor
import model.{ClosestNeighborPoints, PositionInRouteAndRouteNr, HopDistanceAsObjective, VRP}
import neighborhood._
import oscar.cbls.search.StopWatch

class SmartSearch(panelVRP:PanelVRP) extends Runnable with StopWatch{
  var actualNeighbor = 0
  var ended = false

  var mySmartSolution:SmartSolution=null;

  def getSelectedNeighborhood(kLimit:Int,n:Neighbor) = panelVRP.getSelectedNeighborhood(kLimit,n)

  def updateVisualisation(iteration:Int) = panelVRP.updateVisualisation(iteration)

  def getImprovingNeighbor(vrp:VRP with HopDistanceAsObjective with PositionInRouteAndRouteNr
    with ClosestNeighborPoints,kLimited:Int,previousMove:Neighbor):Neighbor =  {
     actualNeighbor match{
      case 0 => {
        val n = OnePointMove.getFirstImprovingMove(vrp,startFrom=previousMove)
        if (n==null) actualNeighbor += 1;n}
      case 1 => {
        val n = Swap.getFirstImprovingMove(vrp,vrp.getKNearest(kLimited),previousMove)
        if (n==null) actualNeighbor += 1;n}
      case 2 => {
        val n = ThreeOptA.getFirstImprovingMove(vrp,vrp.getKNearest(kLimited),previousMove)
        if (n==null) actualNeighbor += 1;n}
      case 3 => {
        val n = ThreeOptB.getFirstImprovingMove(vrp,vrp.getKNearest(kLimited),previousMove)
        if (n==null) actualNeighbor += 1;n}
      case 4 => {
        val n = TwoOpt.getFirstImprovingMove(vrp,vrp.getKNearest(kLimited),previousMove)
        if (n==null){ended =true} else actualNeighbor = 0;n}
      }
  }

  def run(){
    var smartIt = 1
    var bestObj = panelVRP.vrpModel.vrp.ObjectiveVar.value

    while(true){
      ended =false
      val kLimited = panelVRP.boardPanel.klimited.getText.toInt
      val boardPanel = panelVRP.boardPanel
      val vrp = panelVRP.vrpModel.vrp
      RandomNeighbor(vrp)
      var previousMove:Neighbor = null
      var it = panelVRP.vrpModel.it

      val startObjective = vrp.ObjectiveVar.value - vrp.AddedObjectiveFunctions.value
      startWatch() // timer
      while(!ended){
        if(boardPanel.inPause || boardPanel.inIteration() ){ // visual interface
          boardPanel.unlock2()
          boardPanel.lock()
          smartIt=1;
        }

        val oldObj:Int = vrp.ObjectiveVar.value

        previousMove = getImprovingNeighbor(vrp,kLimited,previousMove)
        if ((previousMove != null && previousMove.getObjAfter < oldObj) ){
          it += 1
          previousMove.comit
          updateVisualisation(it)
        }
      }

      boardPanel.iteration = false
      boardPanel.start.setText("Start")
      panelVRP.vrpModel.it = 0
      if (panelVRP.vrpModel.vrp.ObjectiveVar.value < bestObj){
        bestObj = panelVRP.vrpModel.vrp.ObjectiveVar.value
        println("Nouveau meilleur objectif: \n" +
          "-----------   ite: "+smartIt+"\n"+
          "-----------   obj: "+bestObj+"\n")

        if(smartIt == 1) mySmartSolution = new SmartSolution(panelVRP)
        else mySmartSolution.updateUI()
      }
      smartIt = smartIt +1
    }
  }
}
