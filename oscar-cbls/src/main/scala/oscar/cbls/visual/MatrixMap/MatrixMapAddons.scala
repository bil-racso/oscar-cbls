package oscar.examples.cbls.routing.visual.MatrixMap

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
