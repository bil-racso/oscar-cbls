package oscar.cbls.visual.MatrixMap

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

import java.awt._
import java.awt.event.{ItemEvent, ItemListener, MouseEvent, MouseListener}
import javax.swing.{BoxLayout, JCheckBox, JPanel}

import oscar.cbls.invariants.lib.logic.Cluster
import oscar.cbls.routing.seq.model.{PDP, VRP}
import oscar.visual.shapes.VisualCircle

import scala.List

/**
  * Created by fabian on 23-02-16.
  */
trait PickupAndDeliveryPoints extends BasicRoutingMap{

  override def drawPoints(): Unit ={
    var v = this.v
    for(p <- pointsList){
      if(v > 0){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,5)
        tempPoint.innerCol_$eq(colorValues(v))
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,4)
        tempPoint.innerCol_$eq(Color.black)
        tempPoint.toolTip_=(getPointInformation(p))
      }
      v -= 1
    }
  }

  def getPointInformation(point:(Int,Int)): String ={
    "Some informations about the point"
  }
}

trait GeoPickupAndDeliveryPoints extends GeoRoutingMap{
  def pdp:PDP
  require(pdp != null, "In order to use the GeoPickupAndDeliveryPoints trait you must specify a PDP object.")

  //val cluster = Cluster.MakeDenseAssumingMinMax(pdp.arrivalTime, 0, 86400)

  getMainMap.addMouseListener(new MouseListener {
    override def mouseExited(e: MouseEvent): Unit = {}

    override def mouseClicked(e: MouseEvent): Unit = {
      val rect = getMainMap.getViewportBounds
      for(i <- pointsList.indices){
        val point = new Point(pointsList(i)._1.toInt - rect.x, pointsList(i)._2.toInt - rect.y)
        if(point.distance(e.getPoint) < 10) {
          println("Some informations about the point : " + i)
          println("Arrival time : " + pdp.arrivalTime(i).value)
          println("Leave time : " + pdp.leaveTime(i).value)
          println("Passengers on board : " + pdp.arrivalLoadValue(i).value)
          println()
        }
      }
      println()
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {}

    override def mouseReleased(e: MouseEvent): Unit = {}
  })
}

trait RouteToDisplay extends RoutingMap{
  def container:RoutingMatrixContainer
  require(container != null, "In order to use the RouteToDisplay trait you must specify a RoutingMatrixContainer object.")
  def vrp:VRP
  require(vrp != null, "In order to use the RouteToDisplay trait you must specify a VRP object.")

  val routesToDisplay:Array[Boolean] = Array.tabulate(vrp.v)(v =>false)

  val vehicleSelectionPane = new JPanel()
  vehicleSelectionPane.setLayout(new BoxLayout(vehicleSelectionPane,BoxLayout.Y_AXIS))
  val allCheckBox = new JCheckBox("All")
  allCheckBox.addItemListener(new ItemListener {
    override def itemStateChanged(e: ItemEvent): Unit = {
      for(c <- vehicleSelectionPane.getComponents) {
        val box = c.asInstanceOf[JCheckBox]
        box.setSelected(e.getStateChange == ItemEvent.SELECTED)
      }
    }
  })
  vehicleSelectionPane.add(allCheckBox)
  for (i <- 0 until vrp.v){
    val checkBox = new JCheckBox("Vehicle : " + i)
    checkBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent): Unit = {
        routesToDisplay(i) = e.getStateChange == ItemEvent.SELECTED
        drawRoutes(container.allRoutes,routesToDisplay)
      }
    })
    vehicleSelectionPane.add(checkBox)
  }
  container.add(vehicleSelectionPane, BorderLayout.EAST)
}
