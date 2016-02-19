package oscar.examples.cbls.routing.visual

import java.awt.Color
import java.awt.geom.Line2D.Double

import oscar.cbls.routing.model.VRP
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualLine, VisualCircle}

import scala.collection.mutable.ListBuffer
import scala.swing._

/**
  * @author fabian.germeau@student.vinci.be
  */
class RoutingMatrixMap extends VisualDrawing(false,false){

  var pointsList:scala.List[(Int, Int)] = Nil
  var colorValues:Array[Color] = null
  var vrp:VRP = null
  var mapSize = 0

  def drawPoints() ={
    for(p <- pointsList){
      if(pointsList.indexOf(p) < vrp.V){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,5)
        tempPoint.innerCol_$eq(colorValues(pointsList.indexOf(p)))
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,2)
        tempPoint.innerCol_$eq(Color.black)
      }
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
        val tempRoute = new VisualLine(this,new Double(pointsList(old)._1, pointsList(old)._2,pointsList(p)._1,pointsList(p)._2))
        tempRoute.outerCol_$eq(color)
        old = p
      }
      val tempRoute = new VisualLine(this,new Double(pointsList(old)._1, pointsList(old)._2,pointsList(points.head)._1,pointsList(points.head)._2))
      tempRoute.outerCol_$eq(color)
    }
  }

  def setVRP(vrp:VRP): Unit ={
    this.vrp = vrp
  }

  def setColorValues(colorValues:Array[Color]): Unit ={
    if(colorValues == null){
      this.colorValues = RandomColorGenerator.generateRandomColors(vrp.V)
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
