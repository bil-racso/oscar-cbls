package oscar.examples.cbls.routing.visual.ObjFunctionCurve

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
import java.awt.event.{AdjustmentEvent, AdjustmentListener}
import javax.swing.{SwingConstants, JScrollBar}

/**
  * Created by fabian on 22-02-16.
  */

trait RightLeftScrollbar extends ObjFunctionGraphic {
  //Determine the border of the X axis
  override def setTimeBorders(position:Int = 0): Unit ={
    minObjTime = position*100
    maxObjTime = minObjTime + maxWidth - minWidth
    maxNumberOfObj = objValues.length - objTimes.indexWhere(_ > minObjTime)
    drawObjectiveCurve()
  }
}

trait AdjustDisplayedValue extends ObjFunctionGraphic{
  //Determine the number of values that will be displayed
  override def setMaxNumberOfObject(percentage:scala.Double = 0.9): Unit ={
    maxNumberOfObj = (objValues.length*percentage).toInt
    minObjTime = objTimes(objValues.length - maxNumberOfObj)
    maxObjTime = minObjTime + maxWidth - minWidth
    drawObjectiveCurve()
  }
}
