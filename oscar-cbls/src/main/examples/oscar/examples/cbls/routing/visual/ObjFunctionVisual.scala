package oscar.examples.cbls.routing.visual

import java.awt.event.{AdjustmentEvent, AdjustmentListener}
import java.awt.{BorderLayout, Toolkit}
import javax.swing._

import oscar.visual.VisualFrame

import scala.swing.Dimension

/**
  * Created by fabian on 16-02-16.
  */
trait ObjFunctionVisual {
  val objGraphic = new ObjFunctionGraphic()

  def saveObjValue(objValue:Int,objTime:Long): Unit = {
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

  override def saveObjValue(objValue:Int,objTime:Long): Unit ={
    super.saveObjValue(objValue,objTime)
    objGraphic.validate()
  }
}

class InternalObjFunctionVisual(title:String = "Evolution of the objective function", dimension:Dimension = null) extends JInternalFrame(title) with ObjFunctionVisual{
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
    println(objGraphic.minObjTime + "     " + objGraphic.maxObjTime)
    validate()
  }

  override def clear(): Unit ={
    super.clear()
    validate()
  }

  override def saveObjValue(objValue:Int,objTime:Long): Unit ={
    super.saveObjValue(objValue,objTime)
    validate()
  }

  def adjustScrollBarSize(absMin:Int,absMax:Int): Unit ={
    absScrollBar.setMinimum(absMin)
    absScrollBar.setMaximum(absMax)
    absScrollBar.setValue(absScrollBar.getMaximum)
  }

}
