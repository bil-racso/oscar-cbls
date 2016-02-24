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
  * This file contains all the add-on modules that you could add to your ObjFunctionGraphic object.
  * Each trait override one of the method used in ObjFunctionGraphic.
  * This way you can build any ObjFunctionGraphic object you want just by adding the desired trait on the construction.
  * @author fabian.germeau@student.vinci.be
  */

/**
  * This trait, associated with a scrollbar allow you to move left and right in the drawn curve.
  * This way you can look the information more precisely
  * (otherwise all the curve is drawn which is not very useful if the curve have a very lon X axis)
  */
trait RightLeftScrollbar extends ObjFunctionGraphic {
  /**
    * This method, called by the scrollbar adjust the minObjTime's and the maxObjTime's value
    * in order to show only the objValue contained in this time window
    * @param position the current position of the scrollbar
    */
  override def setTimeBorders(position:Int = 0): Unit ={
    minObjTime = position*100
    maxObjTime = minObjTime + maxWidth - minWidth
    maxNumberOfObj = objValues.length - objTimes.indexWhere(_ > minObjTime)
    drawObjectiveCurve()
  }
}

/**
  * This trait, associated with a scrollbar allow you to reduce/augment the number of value displayed on the curve.
  * So if you reduce the number of value displayed to 90%,
  * only the 90% values with the lower objTime value will be displayed.
  */
trait AdjustDisplayedValue extends ObjFunctionGraphic{
  /**
    * This method, called by the scrollbar adjust the maxNumberOfObj's, the minObjTime's and the maxObjTime's value
    * in order to reduce/augment the maximum objective value that may be displayed.
    * MinObjTime and maxObjTime are adjusted so that there is always something displayed
    * (otherwise you could have a blank)
    * @param percentage the percentage of value that may be displayed
    */
  override def setMaxNumberOfObject(percentage:scala.Double = 0.9): Unit ={
    maxNumberOfObj = (objValues.length*percentage).toInt
    minObjTime = objTimes(objValues.length - maxNumberOfObj)
    maxObjTime = minObjTime + maxWidth - minWidth
    drawObjectiveCurve()
  }
}
