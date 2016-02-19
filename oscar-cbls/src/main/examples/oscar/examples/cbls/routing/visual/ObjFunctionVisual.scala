package oscar.examples.cbls.routing.visual

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

import oscar.visual.VisualFrame

import scala.swing.Dimension

/**
  * @author fabian.germeau@student.vinci.be
  */
trait ObjFunctionVisual {
  val objGraphic = new ObjFunctionGraphic()

  def notifyValue(objValue:Int, objTime:Long): Unit = {
    objGraphic.saveObjValue(objTime,objValue)
  }

  def clear(): Unit = {
    objGraphic.clear()
  }

  def drawGlobalCurve(): Unit ={
    println("drawing")
    objGraphic.drawGlobalCurve()
  }
}


class FramedObjFunctionVisual(title:String = "Evolution of the objective function", dimension:Dimension = null) extends VisualFrame(title) with ObjFunctionVisual{

  val sZ = Toolkit.getDefaultToolkit.getScreenSize
  if(dimension == null)
    setSize(new Dimension(sZ.getWidth.toInt/2,sZ.getHeight.toInt/2))
  else
    setSize(dimension)
  setLayout(new BorderLayout())
  add(objGraphic, BorderLayout.CENTER)

  override def drawGlobalCurve(): Unit ={
    super.drawGlobalCurve()
    objGraphic.validate()
    println("Drawing done and validated")
  }

  override def clear(): Unit ={
    super.clear()
    objGraphic.validate()
  }

  override def notifyValue(objValue:Int, objTime:Long): Unit ={
    super.notifyValue(objValue,objTime)
    objGraphic.validate()
  }
}

class InternalObjFunctionVisual(title:String = "Evolution of the objective function") extends JInternalFrame(title) with ObjFunctionVisual{
  setLayout(new BorderLayout())
  add(objGraphic, BorderLayout.CENTER)
  setVisible(true)

  val ordScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  ordScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      objGraphic.setMaxNumberOfObject((ordScrollBar.getMaximum - e.getValue)/ordScrollBar.getMaximum.toDouble)
    }
  })
  add(ordScrollBar, BorderLayout.EAST)
  val absScrollBar = new JScrollBar(SwingConstants.HORIZONTAL,0,1,0,500)
  absScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      objGraphic.setTimeBorders(e.getValue)
    }
  })
  add(absScrollBar, BorderLayout.SOUTH)


  override def drawGlobalCurve(): Unit ={
    super.drawGlobalCurve()
    adjustScrollBarSize((objGraphic.minObjTime/100).toInt,(objGraphic.maxObjTime/100).toInt)
    validate()
  }

  override def clear(): Unit ={
    super.clear()
    validate()
  }

  override def notifyValue(objValue:Int, objTime:Long): Unit ={
    super.notifyValue(objValue,objTime)
    validate()
  }

  def adjustScrollBarSize(absMin:Int,absMax:Int): Unit ={
    absScrollBar.setMinimum(absMin)
    absScrollBar.setMaximum(absMax)
    absScrollBar.setValue(absScrollBar.getMaximum)
  }

}
