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

import java.awt.{Color, BorderLayout}
import java.awt.event.{FocusEvent, FocusListener, AdjustmentListener, AdjustmentEvent}
import javax.swing.{JLabel, JFormattedTextField, SwingConstants, JScrollBar}

/**
  * This file contains all the add-on modules that you could add to your FunctionGraphicContainer object.
  * This way you can build any FunctionGraphicContainer object you want just by adding the desired trait on the construction.
  *
  * @author fabian.germeau@student.vinci.be
  */

//TODO: Javadoc
trait Zoom extends ObjFunctionGraphicContainer{
  val zoomScrollBar = new JScrollBar(SwingConstants.VERTICAL,100,1,0,101)
  zoomScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.heightAdapter = Math.max(1,Math.ceil((graphic.diffYValue()/ graphic.diffHeight().toDouble)*getLogZoom(zoomScrollBar.getValue)/100))
      graphic.timeUnit = Math.max(1,Math.ceil((graphic.maxXValue()/ graphic.diffWidth().toDouble)*getLogZoom(zoomScrollBar.getValue)/100))
      adjustScrollBar()
      graphic.minXValueDisplayed = graphic.timeUnit.toLong * rightLeftScrollBar.getValue
      graphic.maxXValueDisplayed = graphic.minXValueDisplayed + (graphic.timeUnit.toLong * graphic.diffWidth())
      graphic.minYValueDisplayed = graphic.heightAdapter.toLong * upDownScrollBar.getValue
      graphic.maxYValueDisplayed = graphic.minYValueDisplayed + (graphic.heightAdapter.toLong * graphic.diffHeight())
      graphic.drawGlobalCurve()

      def getLogZoom(i:Double): Double ={
        (100*Math.pow(Math.log(i),4)/Math.pow(Math.log(100),4))
      }
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

  val neighborhoodColorLabel = new JLabel(" ")
  neighborhoodColorLabel.setHorizontalAlignment(SwingConstants.CENTER)
  add(neighborhoodColorLabel, BorderLayout.NORTH)

  //TODO: Javadoc
  def adjustScrollBar(): Unit ={
    rightLeftScrollBar.setMaximum(Math.max(
    0,Math.ceil(
        (graphic.maxXValue()/graphic.timeUnit)- graphic.diffWidth()).toInt))
    rightLeftScrollBar.setUnitIncrement(rightLeftScrollBar.getMaximum/100)
    upDownScrollBar.setMaximum(Math.max(
    0,Math.ceil(
        (graphic.diffYValue()/graphic.heightAdapter)- graphic.diffHeight()).toInt))
    upDownScrollBar.setUnitIncrement(upDownScrollBar.getMaximum/100)

  }

  //TODO: Javadoc
  override def drawGlobalCurve(): Unit ={
    graphic.drawGlobalCurve()
    zoomScrollBar.setValue(100)
    var labelText = "<html>"
    for(k <- graphic.xColorMap.keys){
      val r = graphic.xColorMap.get(k).get.getRed
      val g = graphic.xColorMap.get(k).get.getGreen
      val b = graphic.xColorMap.get(k).get.getBlue
      labelText = labelText + "<font color=rgb("+r+","+g+","+b+")>" + k + "    " + "</font>"
    }
    labelText = labelText + "</html>"
    neighborhoodColorLabel.setText(labelText)
  }
}

