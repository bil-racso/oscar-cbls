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

//TODO: Javadoc
trait Zoom extends ObjFunctionGraphicContainer{
  val zoomScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  zoomScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.heightAdapter = Math.max(1,(graphic.diffYValue()/ graphic.diffHeight())*zoomScrollBar.getValue/100)
      graphic.timeUnit = Math.max(1,Math.ceil((graphic.maxXValue()/ graphic.diffWidth())*zoomScrollBar.getValue/100))
      adjustScrollBar()
      graphic.minXValueDisplayed = graphic.timeUnit.toLong * rightLeftScrollBar.getValue
      graphic.maxXValueDisplayed = graphic.minXValueDisplayed + (graphic.timeUnit.toLong * graphic.diffWidth())
      graphic.minYValueDisplayed = graphic.heightAdapter.toLong * upDownScrollBar.getValue
      graphic.maxYValueDisplayed = graphic.minYValueDisplayed + (graphic.heightAdapter.toLong * graphic.diffHeight())
      graphic.drawGlobalCurve()
    }
  })
  add(zoomScrollBar, BorderLayout.EAST)

  val rightLeftScrollBar = new JScrollBar(SwingConstants.HORIZONTAL,0,1,0,100)
  rightLeftScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minXValueDisplayed = graphic.timeUnit.toLong * rightLeftScrollBar.getValue
      graphic.maxXValueDisplayed = graphic.minXValueDisplayed + (graphic.timeUnit.toLong * graphic.diffWidth())
      graphic.drawGlobalCurve()
    }
  })
  add(rightLeftScrollBar, BorderLayout.SOUTH)

  val upDownScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  upDownScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minYValueDisplayed = graphic.minYValue() + (graphic.heightAdapter.toLong * (upDownScrollBar.getMaximum - upDownScrollBar.getValue))
      graphic.maxYValueDisplayed = graphic.minYValueDisplayed + (graphic.heightAdapter.toLong * graphic.diffHeight())
      graphic.drawGlobalCurve()
    }
  })
  add(upDownScrollBar, BorderLayout.WEST)

  def adjustScrollBar(): Unit ={
    rightLeftScrollBar.setMaximum(Math.max(
      0,Math.ceil(
        (graphic.maxXValue()/graphic.timeUnit)- graphic.diffWidth()).toInt))
    upDownScrollBar.setMaximum(Math.max(
      0,Math.ceil(
        (graphic.maxYValue()/graphic.heightAdapter)- graphic.diffHeight()).toInt))
  }

  override def drawGlobalCurve(): Unit ={
    adjustScrollBar()
    graphic.drawGlobalCurve()
  }
}
