package oscar.examples.cbls.routing.visual.MatrixMap

import java.awt.Color
import javax.swing.JOptionPane

import oscar.cbls.routing.model.{VehicleWithCapacity, VRP, PickupAndDeliveryCustomers}
import oscar.visual.shapes.VisualCircle

/**
  * Created by fabian on 23-02-16.
  */
trait PickupAndDeliveryPoints extends  MatrixMap{

  var pdptwVRP:VRP with PickupAndDeliveryCustomers with VehicleWithCapacity = null
  var pickupNodeCircle:VisualCircle = null
  var deliveryNodeCircle:VisualCircle = null

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
      case pdptwVRP:VRP with PickupAndDeliveryCustomers with VehicleWithCapacity => this.pdptwVRP = pdptwVRP
      case _ => assert(false,"If you use this trait, the vrp set has to be instantiate with the PickupAndDeliveryCustomers trait")
    }
    super.setVRP(vrp)
  }

  private def getPointInformation(index:Int): String ={
    vrp.getNodeInformation(index) + "\n " + pdptwVRP.currentLoad(index)
  }

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
