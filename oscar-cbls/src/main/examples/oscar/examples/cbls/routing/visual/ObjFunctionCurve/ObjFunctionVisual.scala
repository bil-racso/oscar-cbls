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
import java.awt.{BorderLayout, Toolkit}
import javax.swing._

import scala.swing.Dimension

/**
  * @author fabian.germeau@student.vinci.be
  */

class InternalObjFunctionVisual(title:String = "Evolution of the objective function",
                                rightLeftScrollbar:Boolean = false,
                                adjustDisplayedValue:Boolean = false
                               ) extends JInternalFrame(title){

  setLayout(new BorderLayout())
  setVisible(true)

  var objGraphic:ObjFunctionGraphic = null
  val absScrollBar = new JScrollBar(SwingConstants.HORIZONTAL,0,1,0,500)
  absScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      objGraphic.setTimeBorders(e.getValue)
    }
  })
  val ordScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  ordScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      objGraphic.setMaxNumberOfObject((ordScrollBar.getMaximum - e.getValue)/ordScrollBar.getMaximum.toDouble)
    }
  })

  if(rightLeftScrollbar) {
    if(adjustDisplayedValue){
      objGraphic = new ObjFunctionGraphicImpl with RightLeftScrollbar with AdjustDisplayedValue
      add(ordScrollBar, BorderLayout.EAST)
    }else
        objGraphic = new ObjFunctionGraphicImpl with RightLeftScrollbar
    add(absScrollBar, BorderLayout.SOUTH)
  }else if(adjustDisplayedValue) {
    objGraphic = new ObjFunctionGraphicImpl with AdjustDisplayedValue
    add(ordScrollBar, BorderLayout.EAST)
  }else
    objGraphic = new ObjFunctionGraphicImpl

  objGraphic.setSize(getSize())
  objGraphic.setPreferredSize(getSize())
  add(objGraphic, BorderLayout.CENTER)
  validate()

  override def setSize(dimension: Dimension): Unit ={
    super.setSize(dimension)
  }

  def drawGlobalCurve(): Unit ={
    adjustScrollBarSize((objGraphic.minObjTime/100).toInt,(objGraphic.maxObjTime/100).toInt)
    objGraphic.drawGlobalCurve()
    objGraphic.validate()
    validate()
  }

  def clear(): Unit ={
    objGraphic.clear()
    validate()
  }

  def notifyNewObjectiveValue(objValue:Int, objTime:Long): Unit ={
    objGraphic.notifyNewObjectiveValue(objValue,objTime)
    validate()
  }

  //Adjust the size of the ScrollBar
  def adjustScrollBarSize(absMin:Int,absMax:Int): Unit ={
    absScrollBar.setMinimum(absMin)
    absScrollBar.setMaximum(absMax)
    absScrollBar.setValue(absScrollBar.getMaximum)
  }
}
