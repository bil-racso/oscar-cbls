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
package oscar.visual.shapes

import java.awt.Color
import java.awt.geom.RoundRectangle2D
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame

/**
 *
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 *
 */
class VisualLabelledRoundRectangle(d: VisualDrawing, s: RoundRectangle2D.Double, label: String, _marginWidth: Double = 5) extends VisualRoundRectangle(d, s) {

  def rect: RoundRectangle2D.Double = shape
  val textDraw = new VisualText(d, (x + marginWidth).toInt, (y + marginWidth + d.getFontMetrics(d.getFont()).getHeight()).toInt, label)
  var marginWidth = _marginWidth

  def this(d: VisualDrawing, x: Double, y: Double, label: String, arcw: Double = 7, arch: Double = 7, marginWidth: Double = 5) = {
    this(d, new RoundRectangle2D.Double(x,
        y,
        d.getFontMetrics(d.getFont()).stringWidth(label) + marginWidth * 2,
        d.getFontMetrics(d.getFont()).getHeight() + marginWidth * 2,
        arcw,
        arch),
      label,
      marginWidth)
  }

  /**
   * X coordinates of bottom left corner
   * @return
   */
  def xText = (x + marginWidth).toInt

  /**
   * Y coordinates of bottom left corner
   * @return
   */
  def yText = (y + marginWidth + d.getFontMetrics(d.getFont()).getHeight()).toInt

  /**
   * Move the specified left corner
   * @param x
   */
  override def move(x: Double, y: Double) {
    textDraw.move(xText, yText)
    super.move(x, y)
  }

  def getWidth(newLabel: String) = {
    d.getFontMetrics(d.getFont()).stringWidth(newLabel) + marginWidth * 2
  }
}

object VisualLabelledRoundRectangle {

  def main(args: Array[String]) {
    val f = VisualFrame("toto");
    val d = VisualDrawing(false);
    val inf = f.createFrame("Drawing");

    val rect = new VisualLabelledRoundRectangle(d, 50, 50, "I'm a rectangle. Just a rectangle...", 10);
    rect.toolTip = ("Hello");

    inf.add(d);
    f.pack();

    Thread.sleep(1000);
    rect.innerCol = (Color.red);
    Thread.sleep(1000);
    rect.move(100, 20);
    for (i <- 0 until 20) {
      Thread.sleep(50);
      rect.move(rect.x + 5, rect.y);
    }
  }
}
