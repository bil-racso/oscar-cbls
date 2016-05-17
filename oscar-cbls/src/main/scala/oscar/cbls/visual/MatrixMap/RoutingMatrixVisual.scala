package oscar.examples.cbls.routing.visual.MatrixMap

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

import java.awt.BorderLayout
import javax.swing.{JPanel, JInternalFrame}

import oscar.cbls.routing.model.{PDP,VRP}

import scala.swing._

/**
  * This class is directly used by the DemoRoutingView object and indirectly used by the trait in VRP/PDP
  *
  * @author fabian.germeau@student.vinci.be
  */
class RoutingMatrixVisual(title:String = "Routing map", pickupAndDeliveryPoints: Boolean = false) extends JPanel with Runnable{
  setLayout(new BorderLayout())

  val routingMatrix = {
    if(pickupAndDeliveryPoints)
      new RoutingMatrixMap with PickupAndDeliveryPoints
    else
      new RoutingMatrixMap}

  var mustRefresh = false

  /**
    * The run method of the thread, it refresh the map every 100 milli sec
    * If you think that's too much do not hesitate to change it
    */
  def run(): Unit ={
    while (true) {
      try {
        Thread.sleep(100)
        if (mustRefresh) {
          routingMatrix.drawRoutes()
          setMustRefresh(false)
        }
      }catch{
        case ie:InterruptedException => return
        case e:Exception => e.printStackTrace()
      }
    }
  }

  //This method tells whether or not we have to refresh the map
  def setMustRefresh(newVal:Boolean,routes:List[List[Int]] = null): Unit=synchronized{
    assert(routingMatrix.pointsList != Nil,"The list of points is not set")
    assert(routingMatrix.mapSize != 0,"The map size is not set")
    mustRefresh = newVal
    if(newVal)
      routingMatrix.routes = routes
  }

  def drawRoutes(): Unit ={
    routingMatrix.drawRoutes()
    validate()
  }

  def drawPoints(): Unit ={
    routingMatrix.drawPoints()
    validate()
  }

  def setColorValues(colorValues:Array[Color]): Unit ={
    routingMatrix.setColorValues(colorValues)
  }

  def setPointsList(pointsList:List[(Int,Int)]): Unit ={
    routingMatrix.setPointsList(pointsList)
  }

  def setMapSize(mapSize:Int): Unit ={
    routingMatrix.setMapSize(mapSize)
  }

  def setVRP(vrp:VRP): Unit ={
    vrp match{
      case pdptwVRP:PDP => routingMatrix.setVRP(pdptwVRP)
      case _ => routingMatrix.setVRP(vrp)
    }
  }

  def clear(): Unit ={
    routingMatrix.clear()
    validate()
  }
  add(routingMatrix, BorderLayout.CENTER)
  setVisible(true)
}