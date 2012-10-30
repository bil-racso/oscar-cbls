package oscar.visual

import oscar.cp.core.CPVarInt
import java.awt.Color
import oscar.cp.mem.pareto.ParetoSet

import scala.collection.mutable.Queue

class VisualPareto[S](pareto: ParetoSet[S], ref: Array[Int], scale: Int) {

  // Initialize
  // ----------------------------------------------------

  val frame = new VisualFrame("Solution Space")
  val drawing = new VisualDrawing(false)

  frame.createFrame("Pareto").add(drawing)
  frame.pack()

  val zero = 20

  val circles = Queue[VisualCircle]()

  def update {

    drawing.shapes.drop(drawing.shapes.size)

    new VisualLine(drawing, zero, zero, zero + scale, zero)
    new VisualLine(drawing, zero, zero, zero, zero + scale)
    new VisualLine(drawing, zero, zero + scale, zero + scale, zero + scale)
    new VisualLine(drawing, zero + scale, zero, zero + scale, zero + scale)

    val points = pareto.points

    for (i <- 0 until points.size) {
      circles enqueue new VisualCircle(drawing, points(i)(0) * scale / ref(0) + zero, scale - (points(i)(1) * scale / ref(1)) + zero, 2, Color.BLUE)
    }

    drawing.repaint()
  }
}