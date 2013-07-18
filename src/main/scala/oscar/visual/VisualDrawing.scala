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
package oscar.visual

import javax.swing.JPanel
import java.awt.Color
import java.awt.Shape
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.event.MouseMotionListener
import java.awt.event.MouseEvent
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualRectangle
import oscar.visual.shapes.VisualShape
import scala.collection.mutable.Queue

/**
 * VisualDrawing
 *
 *  Contains and draws VisualShapes.
 */
class VisualDrawing(flip: Boolean, translate: Boolean, scale: Boolean) extends JPanel {

  setBackground(Color.white)

  // Shapes contained in the panel
  private val shapes: Queue[VisualShape[Shape]] = Queue()

  private var marginT: Double = 0
  private var marginR: Double = 0
  private var marginB: Double = 0
  private var marginL: Double = 0

  /** Returns the margins of the panel. */
  def margin = (marginT, marginR, marginB, marginL)

  /** Sets the margins of the panel. */
  def margin(m: Double): Unit = margin(m, m, m, m)

  /** Sets the margins of the panel. */
  def margin(top: Double, right: Double, bottom: Double, left: Double): Unit = {
    marginT = top
    marginR = right
    marginB = bottom
    marginL = left
  }

  // Returns the bounds of the bounding box containing all the shapes.
  private def findBounds(shapes: Iterable[VisualShape[Shape]]): (Double, Double, Double, Double) = {
    var minX = Double.MaxValue
    var maxX = Double.MinValue
    var minY = Double.MaxValue
    var maxY = Double.MinValue
    for (shape <- shapes) {
      val bounds = shape.getBounds
      if (bounds._1 < minX) minX = bounds._1
      if (bounds._2 > maxX) maxX = bounds._2
      if (bounds._3 < minY) minY = bounds._3
      if (bounds._4 > maxY) maxY = bounds._4
    }
    (minX, maxX, minY, maxY)
  }

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

      // Flip
      if (flip) {
        g2d.translate(0, dHeight)
        g2d.scale(1, -1)
      }

      // Scale
      if (scale) {
        // Compute the scaling ratio
        val ratioX = dWidth / (marginR + marginL + sWidth)
        val ratioY = dHeight / (marginT + marginB + sHeight)
        val ratio = math.min(ratioX, ratioY) // Maintain proportions
        g2d.scale(ratio, ratio)
      }

      // Translate
      if (translate) {
        val translateX: Int = (marginL - minX).toInt
        val translateY: Int = ((if (flip) marginB else marginT) - minY).toInt 
        g2d.translate(translateX, translateY)
      }

      for (s <- shapes) {
        s.draw(g.asInstanceOf[Graphics2D]);
      }
    }
  }

  /** Adds a new non null colored shape in the panel. */
  def addShape(shape: VisualShape[Shape], repaintAfter: Boolean = true): Unit = {
    if (shape == null) throw new IllegalArgumentException("The added shape is null.")
    else {
      shapes.enqueue(shape)
      if (repaintAfter) repaint()
    }
  }

  /** Removes all the shapes contained in the panel. */
  def clear(repaintAfter: Boolean = true): Unit = {
    shapes.clear()
    revalidate()
    if (repaintAfter) repaint()
  }

  addMouseMotionListener {
    val drawingPanel = this
    new MouseMotionListener() {
      override def mouseMoved(e: MouseEvent) {
        drawingPanel.setToolTipText("");
        for (s <- shapes) {
          s.showToolTip(e.getPoint());
        }
      }
      override def mouseDragged(arg0: MouseEvent) {}
    }
  }

  def showToolTip(text: String): Unit = setToolTipText(text)
}

object VisualDrawing {
  
  def apply(flip: Boolean = true, translate: Boolean = false, scale: Boolean = false): VisualDrawing = {
    new VisualDrawing(flip, translate, scale)
  }
}

object VisualDrawingTest extends App {

  val frame = VisualFrame("Example");
  val drawing = new VisualDrawing(false, true, false);
  val inFrame = frame.createFrame("Drawing");
  inFrame.add(drawing);
  frame.pack();

  val rect = new VisualRectangle(drawing, 50, 50, 100, 100)
  val line = VisualLine(drawing, 50, 50, 150, 150)

  try {
    Thread.sleep(1000);
  } catch {
    case e: InterruptedException => e.printStackTrace();
  }

  rect.innerCol = Color.red;
}

