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
package oscar.visual;

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.Point2D
import oscar.visual._
import java.awt.geom.AffineTransform

class ColoredShape[+E <: Shape](val drawing: VisualDrawing, val shape: E) {

  private var _innerCol = Color.white
  private var _outerCol = Color.black

  private var _visible = true
  private var _fill = true
  private var _border = true

  private var _toolTip: String = null
  def toolTip = _toolTip
  def toolTip_= (s: String): Unit = { _toolTip = s }
  

  drawing.addShape(this);

  def draw(g: Graphics2D) {
    //drawing.scaledTransform.setToIdentity() 
    //val scaledTransform = new AffineTransform();
    //scaledTransform.scale(2, 2);
    //drawing.scaledTransform.setToTranslation(100,100)
    //println(scaledTransform.getTranslateX());
    //g.setTransform(scaledTransform);

    //g.setTransform(drawing.scaledTransform);

    if (visible) {
      if (fill) {
        g.setColor(innerCol)
        g.fill(shape)
      }
      if (border) {
        g.setColor(outerCol)
        g.draw(shape)
      }
    }
  }

  def innerCol = _innerCol
  def outerCol = _outerCol
  def visible = _visible
  def fill = _fill
  def border = _border

  def visible_=(visible: Boolean): Unit = {
    _visible = visible;
    drawing.repaint();
  }

  def border_=(border: Boolean): Unit = {
    _border = border;
    drawing.repaint();
  }

  def fill_=(fill: Boolean): Unit = {
    _fill = fill;
    drawing.repaint();
  }

  def innerCol_=(innerCol: Color): Unit = {
    _innerCol = innerCol;
    drawing.repaint();
  }

  def outerCol_=(outerCol: Color): Unit = {
    _outerCol = outerCol;
    drawing.repaint();
  }

  def showToolTip(mousePoint: Point2D) = {
    if (toolTip != null && shape.contains(mousePoint)) {
    println("IN")
      drawing.showToolTip(toolTip);
    }
  }
}
