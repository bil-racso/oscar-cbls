package oscar.cbls.visual.MatrixMap

/**
  * *****************************************************************************
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
  * ****************************************************************************
  */

import java.awt.{BorderLayout, Color, Dimension}
import java.awt.event.{ItemEvent, ItemListener}
import javax.swing.{BoxLayout, JCheckBox, JFrame, JPanel}

import oscar.cbls.routing.seq.model.VRP


/**
  * @author fabian.germeau@student.vinci.be
  */


class RoutingMatrixVisual(title:String = "Routing map", vrp:VRP = null, pickupAndDeliveryPoints: Boolean = false, geolocalisationMap: Boolean = false) extends JFrame with Runnable{
  setLayout(new BorderLayout())

  var routingMap:JPanel with RoutingMap = _

  (pickupAndDeliveryPoints, geolocalisationMap) match {
    case (false, false) => routingMap = new BasicRoutingMap
    case (true, false) => routingMap = new BasicRoutingMap() with PickupAndDeliveryPoints
    case (false, true) => routingMap = new GeoRoutingMap()
    case (true,true) => routingMap = new GeoRoutingMap() with GeoPickupAndDeliveryPoints
      routingMap.asInstanceOf[GeoRoutingMap with GeoPickupAndDeliveryPoints].setPDP(vrp)
    case _ => ()
  }

  var mustRefresh = false

  var allRoutes:Array[List[Int]] = _

  var routes:List[List[Int]] = Nil

  var routesToDisplay:Array[Boolean] = if(vrp != null)Array.tabulate(vrp.v)(v =>false) else Array.empty

  if(vrp != null) {
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
          routingMap.drawRoutes(allRoutes,routesToDisplay)
        }
      })
      vehicleSelectionPane.add(checkBox)
    }
    add(vehicleSelectionPane, BorderLayout.EAST)
  }

  def run(): Unit ={
    while (true) {
      try {
        Thread.sleep(500)
        if(setMustRefresh(false))
          routingMap.drawRoutes(allRoutes)
      }catch{
        case ie:InterruptedException => return
        case e:Exception => e.printStackTrace()
      }
    }
  }

  def setMustRefresh(toSet:Boolean): Boolean = {
    this.synchronized{
      var res = false
      if(toSet && !mustRefresh)
        mustRefresh = true
      else if(!toSet && mustRefresh) {
        mustRefresh = false
        res = true
      }
      res
    }
  }

  def drawPoints(): Unit ={
    routingMap.drawPoints()
  }

  def setColorValues(colorValues:Array[Color] = null): Unit ={
    routingMap.setColorValues(colorValues)
  }

  def setPointsList(pointsList:List[(Double,Double)],V:Int): Unit ={
    routingMap.setPointsList(pointsList,V)
  }

  def setMapSize(mapSize:Int): Unit ={
    routingMap.setMapSize(mapSize)
  }

  setPreferredSize(new Dimension(960,960))
  add(routingMap, BorderLayout.CENTER)
  pack()
  revalidate()
  setVisible(true)
}
