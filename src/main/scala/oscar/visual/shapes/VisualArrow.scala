package oscar.visual.shapes

import oscar.visual.VisualDrawing
import java.awt.geom.Line2D
import java.awt.Polygon
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import oscar.visual.VisualFrame
import java.awt.Color
import java.awt.BasicStroke

class VisualArrow(d: VisualDrawing, s: Line2D.Double, dim: Int) extends VisualLine(d, s) {

  fill = true
  
  private val arrowHead: Polygon = new Polygon
  arrowHead.addPoint(0, dim)
  arrowHead.addPoint(-dim, -dim)
  arrowHead.addPoint(dim, -dim)

  override def draw(g2d: Graphics2D): Unit = {    
    val oldFill = fill
    fill = false
    super.draw(g2d)
    fill = oldFill
    // Draw the arrow head
    if (visible) {
      drawArrowHead(g2d)
    }
  }

  private def drawArrowHead(g2d: Graphics2D): Unit = {
    // Position of the head
    val transform = new AffineTransform()
    val (x1, y1) = dest
    val (x2, y2) = orig
    val angle: Double = math.atan2(y2 - y1, x2 - x1)
    transform.translate(x2, y2)
    transform.rotate(angle - math.Pi / 2)
    val transformedHead = transform.createTransformedShape(arrowHead)
    // Draw setup
    val oldStroke = g2d.getStroke()
    if (fill) {
      g2d.setColor(innerCol)
      g2d.fill(transformedHead)
    }
    if (border) {
      g2d.setStroke(plainStroke)
      g2d.setColor(outerCol)
      g2d.draw(transformedHead)
      g2d.setStroke(oldStroke)
    }
  }
}

object VisualArrow {

  def apply(drawing: VisualDrawing, xOrig: Double, yOrig: Double, xDest: Double, yDest: Double, dim: Int = 5): VisualArrow = {
    new VisualArrow(drawing, new Line2D.Double(xOrig, yOrig, xDest, yDest), dim)
  }
}

object VisualArrowTest extends App {

  val frame = VisualFrame("Example")
  val drawing = VisualDrawing(false)
  val inFrame = frame.createFrame("Arrow")
  inFrame.add(drawing)
  frame.pack()

  val arrow = VisualArrow(drawing, 50, 50, 100, 50, 5)
  arrow.innerCol = Color.red
  Thread.sleep(1000)
  arrow.dest = (100, 100)
  Thread.sleep(1000)
  arrow.borderWidth = 3
  Thread.sleep(1000)
  arrow.dashed = true
  Thread.sleep(1000)
  arrow.border = false
  Thread.sleep(1000)
  arrow.border = true
  arrow.fill = false
}