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
import javax.swing.{SwingUtilities, JPanel, JInternalFrame}

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation.CBLSSeqVar
import oscar.cbls.routing.model.VRP

import scala.swing._

/**
  * @author fabian.germeau@student.vinci.be
  */


class RoutingMatrixVisual(title:String = "Routing map", pickupAndDeliveryPoints: Boolean = false) extends JPanel with Runnable{
  setLayout(new BorderLayout())

  var routingMatrix:MatrixMap = null
  if(pickupAndDeliveryPoints)
    routingMatrix = new RoutingMatrixMap() with PickupAndDeliveryPoints
  else
    routingMatrix = new RoutingMatrixMap

  var mustRefresh = false

  var allRoutes:CBLSSeqVar = null

  var routes:List[List[Int]] = Nil

  def run(): Unit ={
    while (true) {
      try {
        Thread.sleep(500)
        if(setMustRefresh(false))
          routingMatrix.drawRoutes(allRoutes.value)
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
    routingMatrix.drawPoints()
  }

  def setColorValues(colorValues:Array[Color]): Unit ={
    routingMatrix.setColorValues(colorValues)
  }

  def setPointsList(pointsList:List[(Int,Int)],V:Int): Unit ={
    routingMatrix.setPointsList(pointsList,V)
  }

  def setMapSize(mapSize:Int): Unit ={
    routingMatrix.setMapSize(mapSize)
  }

  def clear(): Unit ={
    routingMatrix.clear()
  }
  add(routingMatrix, BorderLayout.CENTER)
  setVisible(true)

}


class RoutingMatrixVisualWithAttribute(title:String = "Routing map",
                                       vrp:VRP,
                                       mapSize:Int,
                                       pointsList:scala.List[(Int,Int)],
                                       colorValues:Array[Color],
                                       dimension:Dimension = new Dimension(960,960)) extends RoutingMatrixVisual{
  setPreferredSize(dimension)
  setSize(dimension)
  routingMatrix.setPreferredSize(dimension)
  routingMatrix.setSize(dimension)
  routingMatrix.setMapSize(mapSize)
  routingMatrix.setPointsList(pointsList,vrp.V)
  routingMatrix.setColorValues(colorValues)
}
