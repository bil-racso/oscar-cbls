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

import javax.swing.JPanel
import java.awt.Color
import java.awt.Shape
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.BorderLayout
import java.awt.event.MouseMotionListener
import java.awt.FlowLayout
import java.awt.geom.Rectangle2D
import java.awt.geom.Line2D
import java.awt.event.MouseEvent

class VisualDrawing(flip: Boolean, translate: Boolean, scale: Boolean) extends JPanel(new BorderLayout()) {

  // For backward compatibility
  def this(flip: Boolean) = this(flip, false, false)
  
  setBackground(Color.white)

  // Shapes contained in the panel
  private var shapes: Array[ColoredShape[Shape]] = Array() // TODO change the structure

  private var marginT: Double = 0
  private var marginR: Double = 0
  private var marginB: Double = 0
  private var marginL: Double = 0

  /** Returns the margins of the panel. */
  def margin = (marginT, marginR, marginB, marginL)

  /** Sets the margins of the panel. */
  def margin(top: Double, right: Double, bottom: Double, left: Double): Unit = {
    marginT = top
    marginR = right
    marginB = bottom
    marginL = left
  }

  /** Sets the margins of the panel. */
  def margin(m: Double): Unit = margin(m, m, m, m)

  // Returns the bounds of the bounding box containing all the shapes.
  private def findBounds(shapes: Array[ColoredShape[Shape]]): (Double, Double, Double, Double) = {
    var minX = Double.MaxValue
    var maxX = Double.MinValue
    var minY = Double.MaxValue
    var maxY = Double.MinValue
    for (shape <- shapes) {
      val bounds = shape.shape.getBounds()
      if (bounds.x < minX) minX = bounds.x
      if (bounds.x + bounds.width > maxX) maxX = bounds.x + bounds.width
      if (bounds.y < minY) minY = bounds.y
      if (bounds.y + bounds.height > maxY) maxY = bounds.y + bounds.height
    }
    (minX, maxX, minY, maxY)
  }

  // TODO this should be done directly in VisualDrawing
  val drawingPanel: JPanel = new JPanel() {
    override def paint(g: Graphics): Unit = {

      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]

      if (!shapes.isEmpty) {

        // Shapes size
        val (minX, maxX, minY, maxY) = findBounds(shapes)
        val sWidth = maxX - minX
        val sHeight = maxY - minY

        // Drawing size
        val dWidth = getWidth()
        val dHeight = getHeight()

        // Compute the scaling ratio
        val ratio: Double = if (!scale) 1
        else {
          val ratioX = dWidth / (marginR + marginL + sWidth)
          val ratioY = dHeight / (marginT + marginB + sHeight)
          math.min(ratioX, ratioY) // Maintain proportions
        }

        val translateX: Int = if (!translate) 0
        else (marginL - minX).toInt

        val translateY: Int = if (!translate) 0
        else if (flip) (marginB - minY).toInt
        else (marginT - minY).toInt

        // Flip
        if (flip) {
          g2d.translate(0, dHeight)
          g2d.scale(1, -1)
        }

        // Scale
        if (scale) {
          g2d.scale(ratio, ratio)
        }

        // Translate
        if (translate) {
          g2d.translate(translateX, translateY)
        }

        for (s <- shapes) {
          s.draw(g.asInstanceOf[Graphics2D]);
        }
      }
    }
  }

  /** Adds a new non null colored shape in the panel. */
  def addShape(shape: ColoredShape[Shape], repaintAfter: Boolean = true): Unit = {
    if (shape == null) throw new IllegalArgumentException("The added shape is null.")
    else {
      shapes :+= shape
      if (repaintAfter) repaint() 
    }
  }

  /** Removes all the shapes contained in the panel. */
  def clear(repaintAfter: Boolean = true): Unit = {
    shapes = Array()
    revalidate()
    if (repaintAfter) repaint()
  }

  
  drawingPanel.addMouseMotionListener(new MouseMotionListener() {
    override def mouseMoved(e: MouseEvent) {
      drawingPanel.setToolTipText("");
      for (s <- shapes) {
        s.showToolTip(e.getPoint());
      }
    }

    override def mouseDragged(arg0: MouseEvent) {
    }
  })
  
  def showToolTip(text: String) {
    drawingPanel.setToolTipText(text);
  }

  
  // Should be in a top level container class !
  // ------------------------------------------
  drawingPanel.setBackground(Color.white)
  add(drawingPanel, BorderLayout.CENTER)
  val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  buttonPanel.setBackground(Color.white)
  // ------------------------------------------
}

object VisualDrawingTest {
  def main(args: Array[String]) {

    val f = new VisualFrame("toto");
    val d = new VisualDrawing(false);
    val inf = f.createFrame("Drawing");
    inf.add(d);
    f.pack();
    val r = new Rectangle2D.Double(0, 0, 100, 100);

    val rect = new ColoredShape[Rectangle2D](d, r);
    rect.toolTip = "RECTANGLE"
    val l = new ColoredShape[Line2D](d, new Line2D.Double(0, 0, 100, 100));

    try {
      Thread.sleep(1000);
    } catch {
      case e: InterruptedException => e.printStackTrace();
    }
    rect.innerCol = Color.red;

  }
}
