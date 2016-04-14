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

import java.awt.Color
import java.awt.event.{MouseMotionListener, MouseEvent, MouseListener}
import java.awt.geom.Line2D.Double
import javax.swing.SwingUtilities

import oscar.cbls.routing.model.{PickupAndDeliveryCustomers, VRP}
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualArrow, VisualCircle}

import scala.collection.mutable.ListBuffer

/**
  * @author fabian.germeau@student.vinci.be
  */
abstract class MatrixMap extends VisualDrawing(false,false){

  var pointsList:scala.List[(Int, Int)] = Nil
  var colorValues:Array[Color] = null
  var vrp:VRP = null
  var mapSize = 0

  //We remove the unwanted listener inherited from VisualDrawing
  removeMouseListener(getMouseListeners.head)
  addMouseListener(new MouseListener {override def mouseExited(e: MouseEvent): Unit = {}
    override def mouseClicked(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        shapes.foreach(_.clicked(e.getPoint()))
      }
    }
    override def mouseEntered(e: MouseEvent): Unit = {}
    override def mousePressed(e: MouseEvent): Unit = {}
    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  def drawPoints()

  def drawRoutes()

  def setVRP(vrp:VRP): Unit ={
    this.vrp = vrp
  }

  def setColorValues(colorValues:Array[Color]): Unit ={
    if(colorValues == null){
      this.colorValues = ColorGenerator.generateRandomColors(vrp.V)
    }else{
      this.colorValues = colorValues
    }
  }

  def setPointsList(pointsList:scala.List[(Int,Int)]): Unit ={
    val tempList:ListBuffer[(Int,Int)] = new ListBuffer[(Int,Int)]
    for(p <- pointsList){
      val tempP = (p._1*getHeight/mapSize, p._2*getHeight/mapSize)
      tempList.append(tempP)
    }
    this.pointsList = tempList.toList
  }

  def setMapSize(mapSize:Int): Unit ={
    this.mapSize = mapSize
  }

  def clear(): Unit ={
    super.clear()
  }
}

class RoutingMatrixMap extends MatrixMap{

  def drawPoints() ={
    var v = 0
    for(p <- pointsList){
      if(v < vrp.V){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,6)
        tempPoint.innerCol_$eq(colorValues(v))
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,4)
        tempPoint.innerCol_$eq(Color.black)
      }
      v += 1
    }
  }

  def drawRoutes(): Unit ={
    clear()
    drawPoints()

    val routes = (for(c <- 0 until vrp.V)yield vrp.getRouteOfVehicle(c)).toList
    for(r <- 0 until vrp.V){
      val color:Color = colorValues(r)
      val points = routes(r)
      var old = points.head
      for(p <- points){
        val tempRoute = new VisualArrow(this,new Double(pointsList(old)._1, pointsList(old)._2,pointsList(p)._1,pointsList(p)._2),4)
        tempRoute.outerCol_$eq(color)
        tempRoute.borderWidth = 2
        old = p
      }
      val tempRoute = new VisualArrow(this,new Double(pointsList(old)._1, pointsList(old)._2,pointsList(points.head)._1,pointsList(points.head)._2),4)
      tempRoute.outerCol_$eq(color)
      tempRoute.borderWidth = 2
    }
  }
}
