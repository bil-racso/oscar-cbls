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

import javax.swing.{BorderFactory, JFrame}
import oscar.visual.{VisualLine, VisualArrow, VisualCircle, VisualDrawing}
import java.awt.{Color, Dimension}

object SmartSolution{
  var frame:SmartSolution = null

  def apply(b:Boolean){
    if (b || frame==null) {if(frame != null) frame.dispose(); frame = new SmartSolution()}
    else frame.updateUI()
  }
}

class SmartSolution() extends JFrame{
  val mapPanel : VisualDrawing = new VisualDrawing(false);
  mapPanel.setPreferredSize(new Dimension(700,700))
  mapPanel.setMinimumSize(new Dimension(500,500))
  mapPanel.setBorder(BorderFactory.createTitledBorder("Map"))
  mapPanel.setBackground(Color.white)

  setContentPane(mapPanel)
  setVisible(true)
  setTitle("Best Solution")
  pack()
  var arrows:Array[VisualArrow] = null

  cleanMapPanel()
  displayNodes()
  displayArrows()

  def cleanMapPanel(){
    mapPanel.shapes = Array()
  }

  def setColorToRoute(l:VisualLine ,i:Int){
    l.outerCol = PanelVRP.PanelVRP.colorsManagement(i+1)
  }

  def updateVisualisation() {
    val vrp = PanelVRP.vrpModel.vrp
    val nodes = PanelVRP.vrpModel.towns

    def update(i: Int) {
      if(vrp.isRouted(i)){
        arrows(i).visible = true
        setColorToRoute( arrows(i),vrp.RouteNr(i).value)
        arrows(i).setDest(nodes(vrp.Next(i).value).long,
          nodes(vrp.Next(i).value).lat)
      }
      else
        arrows(i).visible = false
    }
    for (i <- 0 until vrp.N) update(i)
  }



  def displayNodes(){
    val nodes = PanelVRP.vrpModel.towns
    PanelVRP.PanelVRP.colorsManagement.setDifferentColors(PanelVRP.vrpModel.V)
    for(i <- 0 until nodes.length){
      val t = nodes(i)
      if (i<PanelVRP.vrpModel.V)
        new VisualCircle(mapPanel, t.long,t.lat,10,Color.blue).innerCol = PanelVRP.PanelVRP.colorsManagement(i+1)
      else
        new VisualCircle(mapPanel, t.long,t.lat,6,Color.white)
    }
  }

  def displayArrows() {
    val vrp = PanelVRP.vrpModel.vrp
    val nodes = PanelVRP.vrpModel.towns

    arrows = Array.tabulate(PanelVRP.vrpModel.N)(i => {
      val arrow =
        if (vrp.isRouted(i)) new VisualArrow(mapPanel,nodes(i).long,nodes(i).lat,
          nodes(vrp.Next(i).value).long,nodes(vrp.Next(i).value).lat,4)
        else new VisualArrow(mapPanel,nodes(i).long,nodes(i).lat,nodes(i).long,nodes(i).lat,4)
      if(vrp.isRouted(i)) setColorToRoute(arrow,vrp.RouteNr(i).value)
      arrow
    })
  }

  def updateUI(){
    updateVisualisation()
  }
}