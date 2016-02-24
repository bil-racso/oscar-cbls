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
import java.awt.BorderLayout
import javax.swing._

import scala.swing.Dimension

/**
  * This class create the JInternalFrame that will contain
  * the ObjFunctionGraphic and all the add-on related to it.
  * @param title The title of the JInternalFrame
  * @param rightLeftScrollbar If true, a scrollBar will be added at the bottom of the JInternalFrame
  *                           This scrollbar will call the setTimeBorder method defined in the trait RightLeftScrollBar
  * @param adjustDisplayedValue If true, a scrollBar will be added on the right side of the JInternalFrame
  *                             This scrollBar will call the setMaxNumberOfObject method defined in the trait AdjustDisplayedValue
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

  //TODO : Find a better way to create the desired object
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

  /**
    * Adjust the size of the scrollbar with the minObjTime and maxObjTime value
    * @param absMin The minObjTimeValue
    * @param absMax The maxObjTimeValue
    */
  def adjustScrollBarSize(absMin:Int,absMax:Int): Unit ={
    absScrollBar.setMinimum(absMin)
    absScrollBar.setMaximum(Math.max(absMin,absMax-10))
    absScrollBar.setValue(absScrollBar.getMaximum)
  }
}
