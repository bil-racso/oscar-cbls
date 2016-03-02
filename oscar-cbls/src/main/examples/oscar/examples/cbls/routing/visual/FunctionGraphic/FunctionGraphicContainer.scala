package oscar.examples.cbls.routing.visual.FunctionGraphic

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
import javax.swing._

import scala.swing.Dimension

/**
  * This abstract class represent the JInternalFrame that will contain
  * a FunctionGraphic and all the add-on related to it.
  *
  * @param title The title of the JInternalFrame
  * @author fabian.germeau@student.vinci.be
  */

abstract class FunctionGraphicContainer(title:String,dimension: Dimension) extends JInternalFrame(title){

  setSize(dimension)
  setPreferredSize(dimension)
  setLayout(new BorderLayout())
  setVisible(true)

  var graphic: FunctionGraphic = null

  validate()

  def drawGlobalCurve()

  def clear(): Unit ={
    graphic.clear()
    validate()
  }
}


/**
  * This class create the JInternalFrame that will contain
  * the ObjFunctionGraphic and all the add-on related to it.
  *
  * @param title The title of the JInternalFrame
  * @author fabian.germeau@student.vinci.be
  */
class ObjFunctionGraphicContainer(title:String = "Evolution of the objective function", dimension: Dimension, defaultColor:Color = Color.black) extends FunctionGraphicContainer(title, dimension){

  graphic = new ObjFunctionGraphic(getWidth,getHeight)
  add(graphic, BorderLayout.CENTER)

  val neighborhoodColorLabel = new JLabel(" ")
  neighborhoodColorLabel.setHorizontalAlignment(SwingConstants.CENTER)
  add(neighborhoodColorLabel, BorderLayout.NORTH)

  def drawGlobalCurve(): Unit ={
    graphic.drawGlobalCurve()
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

  def notifyNewObjectiveValue(objValue:Int, objTime:Long, color: String): Unit ={
    graphic.notifyNewObjectiveValue(objValue,objTime,color)
  }

  override def clear(): Unit ={
    super.clear()
  }
}
