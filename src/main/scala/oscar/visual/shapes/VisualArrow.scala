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

  private val arrowHead: Polygon = new Polygon
  private var headColor: Color = innerCol
  
  def arrowHeadCol: Color = headColor
  def arrowHeadCol_=(color: Color): Unit = {
    headColor = color
    if (autoRepaint) repaint()
  }

  arrowHead.addPoint(0, dim)
  arrowHead.addPoint(-dim, -dim)
  arrowHead.addPoint(dim, -dim)

  override def draw(g2d: Graphics2D): Unit = {
    super.draw(g2d)
    drawArrowHead(g2d)
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
    g2d.setStroke(plainStroke)
    g2d.setColor(headColor)
    g2d.fill(transformedHead)
    g2d.setColor(outerCol)
    g2d.draw(transformedHead)
    g2d.setStroke(oldStroke)
  }
}

object VisualArrow {

  def apply(drawing: VisualDrawing, xOrig: Double, yOrig: Double, xDest: Double, yDest: Double, dim: Int): VisualArrow = {
    new VisualArrow(drawing, new Line2D.Double(xOrig, yOrig, xDest, yDest), dim)
  }
}

object VisualArrowTest extends App {
  
  val frame = new VisualFrame("Example")
  val drawing = new VisualDrawing(false)
  val inFrame = frame.createFrame("Arrow")
  inFrame.add(drawing)
  frame.pack()
  
  val arrow = VisualArrow(drawing, 50, 50, 100, 50, 5)
  arrow.arrowHeadCol = Color.red
  Thread.sleep(1000)
  arrow.dest = (100, 100)
  Thread.sleep(1000)
  arrow.borderWidth = 3
  Thread.sleep(1000)
  arrow.dashed = true
}