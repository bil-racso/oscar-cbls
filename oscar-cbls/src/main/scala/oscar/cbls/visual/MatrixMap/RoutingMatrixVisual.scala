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
import javax.swing.JInternalFrame

import oscar.cbls.routing.model.VRP

import scala.swing._

/**
  * @author fabian.germeau@student.vinci.be
  */


class RoutingMatrixVisual(title:String = "Routing map", pickupAndDeliveryPoints: Boolean = false) extends JInternalFrame(title){
  setLayout(new BorderLayout())

  var routingMatrix:MatrixMap = null
  if(pickupAndDeliveryPoints)
    routingMatrix = new RoutingMatrixMap() with PickupAndDeliveryPoints
  else
    routingMatrix = new RoutingMatrixMap

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
    routingMatrix.setVRP(vrp)
  }

  def clear(): Unit ={
    routingMatrix.clear()
    validate()
  }
  add(routingMatrix, BorderLayout.CENTER)
  setVisible(true)
}


class RoutingMatrixVisualWithAttribute(title:String = "Routing map",
                                       vrp:VRP,
                                       mapSize:Int,
                                       pointsList:scala.List[(Int,Int)],
                                       colorValues:Array[Color],
                                       dimension:Dimension = new Dimension(960,540)) extends RoutingMatrixVisual{
  setPreferredSize(dimension)
  setSize(dimension)
  routingMatrix.setPreferredSize(dimension)
  routingMatrix.setSize(dimension)
  routingMatrix.setVRP(vrp)
  routingMatrix.setMapSize(mapSize)
  routingMatrix.setPointsList(pointsList)
  routingMatrix.setColorValues(colorValues)
}
