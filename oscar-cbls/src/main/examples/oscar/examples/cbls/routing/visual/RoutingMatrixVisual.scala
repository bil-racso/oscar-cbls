package oscar.examples.cbls.routing.visual

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

import java.awt.{BorderLayout, Toolkit}
import javax.swing.JInternalFrame

import oscar.cbls.routing.model.VRP
import oscar.visual.VisualFrame

import scala.swing._

/**
  * @author fabian.germeau@student.vinci.be
  */
trait RoutingMatrixVisual {
  val routingMatrix = new RoutingMatrixMap

  def drawRoutes(): Unit ={
    routingMatrix.drawRoutes()
  }

  def drawPoints(): Unit ={
    routingMatrix.drawPoints()
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
  }
}

class FramedRoutingMatrixVisual(vrp:VRP, mapSize:Int, pointsList:scala.List[(Int,Int)], title:String = "Routing map", dimension:Dimension = null, colorValues:Array[Color] = null) extends VisualFrame(title) with RoutingMatrixVisual{
  val sZ = Toolkit.getDefaultToolkit.getScreenSize
  if(dimension == null)
    setSize(new Dimension(sZ.getWidth.toInt/2,sZ.getHeight.toInt/2))
  else
    setSize(dimension)
  setLayout(new BorderLayout())
  add(routingMatrix, BorderLayout.CENTER)

  /** DO NOT reverse this two lines (routingMatrix needs
    *   the vrp to generate the colors if colorValues is null)
    */
  setVRP(vrp)
  setColorValues(colorValues)
  setMapSize(mapSize)
  setPointsList(pointsList)

  override def drawRoutes(): Unit ={
    super.drawRoutes()
    validate()
  }
}

class InternalRoutingMatrixVisual(title:String = "Routing map") extends JInternalFrame(title) with RoutingMatrixVisual{
  setLayout(new BorderLayout())
  add(routingMatrix, BorderLayout.CENTER)
  setVisible(true)

  /** DO NOT reverse this two lines (routingMatrix needs
    *   the vrp to generate the colors if colorValues is null)
    */

  override def drawRoutes(): Unit ={
    super.drawRoutes()
    validate()
  }

}
