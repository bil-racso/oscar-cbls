package oscar.examples.cbls.routing.visual.MatrixMap

import java.awt.Color
import javax.swing.JOptionPane

import oscar.cbls.routing.model.{VRP, PickupAndDeliveryCustomers}
import oscar.visual.shapes.VisualCircle

/**
  * Created by fabian on 23-02-16.
  */
trait PickupAndDeliveryPoints extends  MatrixMap{

  var pdptwVRP:VRP with PickupAndDeliveryCustomers = null
  var relatedNodeCircle:VisualCircle = null

  override def drawPoints(): Unit ={
    initRelatedNode()
    var v = vrp.V
    for(p <- pointsList){
      if(v > 0){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,6)
        tempPoint.innerCol_$eq(colorValues(v-1))
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,4)
        tempPoint.innerCol_$eq(Color.black)
        tempPoint.onClick(onClickAction(pointsList.indexOf(p)))
        tempPoint.toolTip_=(getPointInformation(pointsList.indexOf(p)))
      }
      v -= 1
    }
  }

  override def setVRP(vrp:VRP): Unit ={
    vrp match{
      case pdptwVRP:VRP with PickupAndDeliveryCustomers => this.pdptwVRP = pdptwVRP
      case _ => assert(false,"If you use this trait, the vrp set has to be instantiate with the PickupAndDeliveryCustomers trait")
    }
    super.setVRP(vrp)
  }

  private def getPointInformation(index:Int): String ={
    vrp.getNodeInformation(index)
  }

  private def onClickAction(index:Int): Unit ={
    var point:(Int,Int) = (-10,-10)
    if(pdptwVRP.isPickup(index))
      point = pointsList(pdptwVRP.getRelatedDelivery(index))
    else if(pdptwVRP.isDelivery(index))
      point = pointsList(pdptwVRP.getRelatedPickup(index))
    relatedNodeCircle.move(point._1,point._2)
  }

  private def initRelatedNode(): Unit ={
    relatedNodeCircle = new VisualCircle(this,-10,-10,10)
    relatedNodeCircle.borderWidth = 2
    relatedNodeCircle.outerCol_$eq(Color.BLUE)
    relatedNodeCircle.fill = false
  }

}
