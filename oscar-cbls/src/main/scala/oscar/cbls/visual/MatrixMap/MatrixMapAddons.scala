package oscar.examples.cbls.routing.visual.MatrixMap

import java.awt.Color
import javax.swing.JOptionPane

import oscar.visual.shapes.VisualCircle

/**
  * Created by fabian on 23-02-16.
  */
trait PickupAndDeliveryPoints extends  MatrixMap{

  override def drawPoints(): Unit ={
    var v = vrp.V
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
