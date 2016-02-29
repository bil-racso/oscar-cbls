package oscar.examples.cbls.routing.visual.FunctionGraphic;

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

import java.awt.BorderLayout
import java.awt.event.{FocusEvent, FocusListener, AdjustmentListener, AdjustmentEvent}
import javax.swing.{JFormattedTextField, SwingConstants, JScrollBar}

/**
  * This file contains all the add-on modules that you could add to your FunctionGraphicContainer object.
  * This way you can build any FunctionGraphicContainer object you want just by adding the desired trait on the construction.
  *
  * @author fabian.germeau@student.vinci.be
  */


/**
 * This trait allow you to move left and right in the drawn curve.
 * This way you can look the information more precisely
 * (otherwise all the curve is drawn which is not very useful if the curve have a very long X axis)
 */
trait RightLeftScrollbar extends ObjFunctionGraphicContainer {
  /**
    * This method, called by the scrollbar adjust the minObjTime's and the maxObjTime's value
    * in order to show only the objValue contained in this time window
    */

  val absScrollBar = new JScrollBar(SwingConstants.HORIZONTAL,0,1,0,500)
  absScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minXValue = e.getValue*100
      graphic.maxXValue = graphic.minXValue + graphic.maxWidth - graphic.minWidth
      graphic.maxNumberOfXYValues = graphic.yValues.length - graphic.xValues.indexWhere(_ > graphic.minXValue)
      drawObjectiveCurve()
    }
  })
  add(absScrollBar, BorderLayout.SOUTH)

  /**
    * Override drawGlobalCurve so that we can adjust the absScrollBar to the size of the graph
    */
  override def drawGlobalCurve(): Unit ={
    println("adjusting scrollbar ...")
    adjustScrollBarSize((graphic.minXValue/100).toInt,(graphic.maxXValue/100).toInt)
    println("scrollbar adjusted")
    super.drawGlobalCurve()
  }

  override def drawObjectiveCurve(): Unit ={
    adjustScrollBarPosition(graphic.minXValue)
    super.drawObjectiveCurve()
  }

  override def clear(): Unit ={
    absScrollBar.setValue(0)
    absScrollBar.setMinimum(0)
    absScrollBar.setMaximum(500)
    super.clear()
  }

  /**
    * Adjust the size of the scrollbar with the minObjTime and maxObjTime value
    *
    * @param absMin The minObjTimeValue
    * @param absMax The maxObjTimeValue
    */
  def adjustScrollBarSize(absMin:Int,absMax:Int): Unit ={
    absScrollBar.setMinimum(absMin)
    absScrollBar.setMaximum(Math.max(absMin,absMax-10))
    absScrollBar.setValue(Math.max(absMin,absMax-10))
  }

  def adjustScrollBarPosition(absMin: Long): Unit ={
    absScrollBar.setValue(Math.max(absScrollBar.getValue,absMin.toInt/100))
  }
}

/**
 * This trait allow you to reduce/augment the number of value displayed on the curve.
 * So if you reduce the number of value displayed to 90%,
 * only the 90% values with the lower objTime value will be displayed.
 */
trait AdjustDisplayedValue extends ObjFunctionGraphicContainer{
  /**
    * This method, called by the scrollbar adjust the maxNumberOfObj's, the minObjTime's and the maxObjTime's value
    * in order to reduce/augment the maximum objective value that may be displayed.
    * MinObjTime and maxObjTime are adjusted so that there is always something displayed
    * (otherwise you could have a blank)
    */
  val ordScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  ordScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      val percentage = (ordScrollBar.getMaximum - e.getValue)/ordScrollBar.getMaximum.toDouble
      graphic.maxNumberOfXYValues = (graphic.yValues.length*percentage).toInt
      graphic.minXValue = graphic.xValues(graphic.yValues.length - graphic.maxNumberOfXYValues)
      graphic.maxXValue = graphic.minXValue + graphic.maxWidth - graphic.minWidth

      drawObjectiveCurve()
    }
  })
  add(ordScrollBar, BorderLayout.EAST)
}


trait AdjustXAxisStepValue extends ObjFunctionGraphicContainer{

  val stepValueTextField = new JFormattedTextField(xAxisStepValue)
  stepValueTextField.setValue(100)
  stepValueTextField.addFocusListener(new FocusListener {
    override def focusGained(e: FocusEvent): Unit = {}

    override def focusLost(e: FocusEvent): Unit = {
    }
  })
  add(stepValueTextField,BorderLayout.NORTH)
}
