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
import neighborhood._
import javax.swing.JOptionPane
import oscar.cbls.search.StopWatch

class SearchVRP(panelVRP:PanelVRP) extends Runnable with StopWatch{


  def getSelectedNeighborhood(kLimit:Int,n:Neighbor) = panelVRP.getSelectedNeighborhood(kLimit,n)
  def updateVisualisation(iteration:Int) = panelVRP.updateVisualisation(iteration)

  def run(){
      val boardPanel = panelVRP.boardPanel
      panelVRP.vrpModel.closeNeighbor = boardPanel.klimited.getText.toInt
      val vrp = panelVRP.vrpModel.vrp
      var previousMove:Neighbor = null
      var it = panelVRP.vrpModel.it
      var ended = false
      val startObjective = vrp.ObjectiveVar.value - vrp.AddedObjectiveFunctions.value
      startWatch() // timer
      while(!ended){
        if(boardPanel.inPause || boardPanel.inIteration() ){ // visual interface
          boardPanel.unlock2()
          boardPanel.lock()
        }


        val oldObj:Int = vrp.ObjectiveVar.value
        previousMove = getSelectedNeighborhood(panelVRP.vrpModel.closeNeighbor,previousMove)
        if ((previousMove != null && previousMove.getObjAfter < oldObj) || previousMove.isInstanceOf[ReinsertPoint]){
          it += 1
          previousMove.comit
          updateVisualisation(it)
          //println("it: " + it + " | objective: "+ vrp.ObjectiveVar.value + " | move: "+previousMove +"\n M(ax)A(vg)U(nrouted)N(eighbor) ="+vrp.maxAvgUnrouted)
        }
        else ended = true
      }
      println("Search ended.")
      val time = getWatch
      JOptionPane.showMessageDialog(null,"Search's strategy is finished. \n" +
        "Objective at start was "+startObjective+
        " and is now "+ (vrp.ObjectiveVar.value - vrp.AddedObjectiveFunctions.value) + " in "+time/1000 +" sec "+ time%1000 + " ms."
        +"\n Total number of iteration is "+it)

      boardPanel.firstIte = true
      boardPanel.pause = true
      boardPanel.iteration = false
      boardPanel.start.setText("Start")
      panelVRP.vrpModel.it = it

    }



}
