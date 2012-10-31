/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */
package oscar.visual;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Point2D;
import oscar.visual._

class ColoredShape[+E <: Shape](_drawing: VisualDrawing, _shape: E) {

  var _innerCol = Color.white
  var _outerCol = Color.black

  var _visible = true
  var _fill = true
  var _border = true

  var toolTip: String = null

  drawing.addShape(this);

  def draw(g: Graphics2D) {
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

  def drawing = _drawing;
  def shape = _shape;

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
      drawing.showToolTip(toolTip);
    }
  }

}
