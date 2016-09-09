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
import java.awt.geom.Line2D.Double

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation.CBLSSeqVar
import oscar.cbls.routing.model.VRP
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualShape, VisualArrow, VisualCircle, VisualLine}

import scala.collection.mutable.ListBuffer

/**
  * @author fabian.germeau@student.vinci.be
  */
abstract class MatrixMap extends VisualDrawing(false,false){

  var pointsList:scala.List[(Int, Int)] = Nil
  var colorValues:Array[Color] = null
  var V:Int = 0
  var mapSize = 10000

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  def drawPoints()

  def drawRoutes(seqRoutes:IntSequence)

  def setColorValues(colorValues:Array[Color]): Unit ={
    if(colorValues == null){
      this.colorValues = ColorGenerator.generateRandomColors(V)
    }else{
      this.colorValues = colorValues
    }
  }

  def setPointsList(pointsList:scala.List[(Int,Int)],V:Int): Unit ={
    this.V = V
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
    var v = V
    for(p <- pointsList){
      if(v > 0){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,5)
        tempPoint.innerCol_$eq(colorValues(pointsList.indexOf(p)))
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,3)
        tempPoint.innerCol_$eq(Color.black)
      }
      v -= 1
    }
  }

  def drawRoutes(seqRoutes:IntSequence): Unit ={
    clear()
    drawPoints()

    val points = seqRoutes.toIterable.toList
    var previousPoint = -1
    var currentVehicle = -1
    var color:Color = null
    for(p <- points){
      if(p >= V && previousPoint != -1){
        val tempRoute = new VisualArrow(this,new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2,pointsList(p)._1,pointsList(p)._2),4)
        tempRoute.outerCol_$eq(color)
        tempRoute.borderWidth = 2
        previousPoint = p
      }
      else if(p < V){
        if(currentVehicle > -1) {
          val tempRoute = new VisualArrow(this, new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2, pointsList(currentVehicle)._1, pointsList(currentVehicle)._2), 4)
          tempRoute.outerCol_$eq(color)
          tempRoute.borderWidth = 2
        }
        previousPoint = p
        currentVehicle = p
        color = colorValues(p)
      }
    }
    val tempRoute = new VisualArrow(this, new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2, pointsList(currentVehicle)._1, pointsList(currentVehicle)._2), 4)
    tempRoute.outerCol_$eq(color)
    tempRoute.borderWidth = 2
  }
}
