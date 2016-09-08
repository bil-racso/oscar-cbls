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
import java.awt.event.{MouseEvent, MouseListener}

import oscar.cbls.routing.seq.model.{PDP, VRP}
import oscar.visual.shapes.VisualCircle

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
  var pdp:PDP = _

  def setPDP(vrp:VRP): Unit ={
    pdp = vrp.asInstanceOf[PDP]
  }

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
