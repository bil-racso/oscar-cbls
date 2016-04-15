package oscar.cbls.visual.FunctionGraphic

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
import java.awt.event.{AdjustmentListener, AdjustmentEvent}
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JSlider, SwingConstants, JScrollBar}

import oscar.cbls.search.algo.LazyQuicksort
import oscar.examples.cbls.routing.visual.FunctionGraphic.ObjFunctionGraphicContainer


/**
  * This file contains all the add-on modules that you could add to your FunctionGraphicContainer object.
  * This way you can build any FunctionGraphicContainer object you want just by adding the desired trait on the construction.
  *
  * @author fabian.germeau@student.vinci.be
  */


/**
  * This trait, attached to a ObjFunctionGraphicContainer add three scrollbar to the graphic.
  * The scrollbar on the right allow the user to zoom in the graphic
  * (quite useful if you want to look closer on specifics parts of the graphic).
  * The scrollbar on the left allow the user to move up and down in the graphic (except when all the graphic is displayed)
  * Similarly the scrollbar on the bottom allow the user to move left and right
  *
  * @author fabian.germeau@student.vinci.be
  */
//TODO : Check the problem when the curve is drawn during the research
trait Zoom extends ObjFunctionGraphicContainer{
  val zoomScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,10,0,101)
  zoomScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      adjustScrollBar()
      graphic.minXValueDisplayed = Math.max(rightLeftScrollBar.getValue,graphic.minXValue())
      graphic.maxXValueDisplayed = Math.min(graphic.minXValueDisplayed+rightLeftScrollBar.getVisibleAmount,graphic.maxXValue())
      graphic.minYValueDisplayed = Math.max(upDownScrollBar.getValue,graphic.minYValue())
      graphic.maxYValueDisplayed = Math.min(graphic.minYValueDisplayed+upDownScrollBar.getVisibleAmount,graphic.maxYValue())
    }
  })
  add(zoomScrollBar, BorderLayout.EAST)

  val rightLeftScrollBar = new JScrollBar(SwingConstants.HORIZONTAL,0,1,0,100)
  rightLeftScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minXValueDisplayed = rightLeftScrollBar.getValue
      graphic.maxXValueDisplayed = graphic.minXValueDisplayed + rightLeftScrollBar.getVisibleAmount
      graphic.drawGlobalCurve()
    }
  })
  add(rightLeftScrollBar, BorderLayout.SOUTH)

  val upDownScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  upDownScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minYValueDisplayed = upDownScrollBar.getMaximum - upDownScrollBar.getVisibleAmount - upDownScrollBar.getValue
      graphic.maxYValueDisplayed = graphic.minYValueDisplayed + upDownScrollBar.getVisibleAmount
      graphic.drawGlobalCurve()
    }
  })
  add(upDownScrollBar, BorderLayout.WEST)

  /*
    This method, called by the zoomScrollBar adjust the values of the rightLeftScrollBar and the upDownScrollBar
    so that their values are adjusted to the new zoom level.
    After adjusting the values, it sets the value of the scrollbar to 0 (top left of the graphic)
   */
  def adjustScrollBar(): Unit ={
    rightLeftScrollBar.setValue(0)
    upDownScrollBar.setValue(0)
    rightLeftScrollBar.setVisibleAmount(Math.max(graphic.diffWidth(), graphic.maxXValue() / (2 * getLogZoom(zoomScrollBar.getValue))).toInt)
    rightLeftScrollBar.setBlockIncrement(rightLeftScrollBar.getVisibleAmount)
    rightLeftScrollBar.setUnitIncrement(rightLeftScrollBar.getVisibleAmount)
    upDownScrollBar.setVisibleAmount(Math.max(20,(graphic.maxYValue()/(2*getLogZoom(zoomScrollBar.getValue))).toInt))
    upDownScrollBar.setBlockIncrement(upDownScrollBar.getVisibleAmount)
    upDownScrollBar.setUnitIncrement(upDownScrollBar.getVisibleAmount)
  }

  /*
    This method set the maximum value of the moving scrollbar to the maximum value of their related axis.
    This way at each position we can easily have the minimum X/Y value to display and, with the extent value
    of the scrollbar, the maximum X/Y value.
   */
  override def drawGlobalCurve(): Unit ={
    rightLeftScrollBar.setMaximum(graphic.maxXValue().toInt+1)
    upDownScrollBar.setMaximum(graphic.maxYValue()+1)
    super.drawGlobalCurve()
  }

  /**
    * This method calculates the new zoom level based on a logarithmic calculation
    *
    * @param i the position of the zoomScrollBar
    * @return the new zoom level
    */
  def getLogZoom(i:Double): Double ={
    if(i == 0)0.5
    else Math.max(1.0,100*Math.pow(Math.log(i), 2) / Math.pow(Math.log(100), 2))
  }
}

/**
  * This trait, attached to a ObjFunctionGraphicContainer add a scrollbar on the right side of the graphic.
  * This scrollbar allow the user to increase or decrease the number of value displayed on the graphic.
  * The number of displayed value is represented in percentages and always begin with the lower value.
  * The minimum value is 1% and the maximum value 100%.
  *
  * @author fabian.germeau@student.vinci.be
  */
trait AdjustMaxValue extends ObjFunctionGraphicContainer{
  var sortedYValues:LazyQuicksort = null

  val adjustMaxValueSlider = new JSlider(SwingConstants.VERTICAL,1,100,100)
  adjustMaxValueSlider.setMajorTickSpacing(10)
  adjustMaxValueSlider.setMinorTickSpacing(1)
  adjustMaxValueSlider.setPaintTicks(true)
  adjustMaxValueSlider.addChangeListener(new ChangeListener {
    /*
      This method starts by sorting a part of the yValues stocked in the graphic object using the LazyQuicksort method.
      Then it selects the last value sorted and set it as the maxYValueDisplayed
      so that only the wanted values will be displayed.
     */
    override def stateChanged(e: ChangeEvent): Unit = {
      sortedYValues = new LazyQuicksort(graphic.yValues.toArray,+_)
      sortedYValues.sortUntil(Math.max(1,(adjustMaxValueSlider.getValue.toDouble/100 * sortedYValues.size).toInt))
      val iteratorXValues = sortedYValues.iterator
      for(i <- 0 until Math.max(1, (adjustMaxValueSlider.getValue.toDouble / 100 * sortedYValues.size).toInt)){
        val temp = iteratorXValues.next()
        if(i == (adjustMaxValueSlider.getValue.toDouble/100 * sortedYValues.size).toInt-1) {
          graphic.maxYValueDisplayed = temp
        }
      }
      graphic.drawGlobalCurve()
    }
  })
  add(adjustMaxValueSlider, BorderLayout.EAST)

  override def drawGlobalCurve(): Unit ={
    super.drawGlobalCurve()
  }
}