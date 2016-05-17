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
package oscar.examples.cbls.routing.visual.MatrixMap

import java.awt.Color
import javax.swing.JOptionPane

import oscar.cbls.routing.model.{PDP, VehicleWithCapacity, VRP}
import oscar.visual.shapes.VisualCircle

/**
  * @author fabian.germeau@student.vinci.be
  */
trait PickupAndDeliveryPoints extends  MatrixMap{

  var pdptwVRP:PDP with VehicleWithCapacity = null
  var pickupNodeCircle:VisualCircle = null
  var deliveryNodeCircle:VisualCircle = null


  /*
   * In order to get information by moving the mouse over a node
   * we have to overrride the drawPoints method
   */
  override def drawPoints(): Unit ={
    initNodeCircles()
    var v = 0
    for(p <- pointsList){
      if(v < pdptwVRP.V){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,6)
        tempPoint.innerCol_$eq(colorValues(v))
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,4)
        tempPoint.innerCol_$eq(Color.black)
        tempPoint.onClick(onClickAction(pointsList.indexOf(p)))
        tempPoint.toolTip_=(getPointInformation(pointsList.indexOf(p)))
      }
      v += 1
    }
  }

  override def setVRP(vrp:VRP): Unit ={
    vrp match{
      case pdptwVRP:PDP with VehicleWithCapacity => this.pdptwVRP = pdptwVRP
      case _ => assert(false,"If you use this trait, the vrp set has to be instantiate with the PickupAndDeliveryCustomers trait")
    }
    super.setVRP(vrp)
  }

  private def getPointInformation(index:Int): String ={
    val info = vrp.getNodeInformation(index).split("\n").foldLeft("")((a,b)=>if(a == "") b else a+"<br>"+b)
    "<html>" + info + "</html>"
  }

  /**
    * Each time we clicked on a node of the map, the related delivery/pickup
    * of this node and this node will be circled
    * @param index
    */
  private def onClickAction(index:Int): Unit ={
    var point:(Int,Int) = (-10,-10)
    if(pdptwVRP.isPickup(index)) {
      point = pointsList(pdptwVRP.getRelatedDelivery(index))
      deliveryNodeCircle.move(point._1,point._2)
      pickupNodeCircle.move(pointsList(index)._1,pointsList(index)._2)
    }
    else if(pdptwVRP.isDelivery(index)) {
      point = pointsList(pdptwVRP.getRelatedPickup(index))
      pickupNodeCircle.move(point._1,point._2)
      deliveryNodeCircle.move(pointsList(index)._1,pointsList(index)._2)
    }else{
      assert(pdptwVRP.isADepot(index),"The node is neither a pickup node nor a delivery node nor a depot")
    }
  }

  /**
    * This method initiate the node circle (used to show where the pickup and delivery nodes are)
    */
  private def initNodeCircles(): Unit ={
    pickupNodeCircle = new VisualCircle(this,-10,-10,10)
    pickupNodeCircle.borderWidth = 2
    pickupNodeCircle.outerCol_$eq(Color.BLUE)
    pickupNodeCircle.fill = false
    deliveryNodeCircle = new VisualCircle(this,-10,-10,10)
    deliveryNodeCircle.borderWidth = 2
    deliveryNodeCircle.outerCol_$eq(Color.RED)
    deliveryNodeCircle.fill = false
  }

}
