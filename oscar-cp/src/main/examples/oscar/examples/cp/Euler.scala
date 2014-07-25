package oscar.examples.cp

import oscar.cp.modeling._
import oscar.algo.search._
import oscar.cp.core._
import oscar.visual._
import java.awt.Color
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualRectangle
import oscar.visual.shapes.VisualCircle

/**
 * Euler Problem, a knight must visit every position of a chess board once and come back to its initial position
 * using only valid knight moves.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Euler extends CPModel with App {

  //  -----------visualization of the euler tour ----------
  val f = new VisualFrame("Euler", 1, 1)
  val drawing = new VisualDrawing(false, true)
  f.createFrame("Euler Tour").add(drawing)
  val scale = 100

  def reachables(i: Int): Set[Int] = {
    def onBoard(y: Int*): Set[Int] = y.filter(x => x >= 0 && x <= 63).toSet
    i % 8 match {
      case 0 => onBoard(i - 15, i - 6, i + 10, i + 17)
      case 1 => onBoard(i - 17, i - 15, i - 6, i + 10, i + 15, i + 17)
      case 6 => onBoard(i - 17, i - 15, i - 10, i + 6, i + 15, i + 17)
      case 7 => onBoard(i - 17, i - 10, i + 6, i + 15)
      case _ => onBoard(i - 17, i - 15, i - 10, i - 6, i + 6, i + 10, i + 15, i + 17)
    }
  }

  val x = (0 until 64).map(v => CPIntVar(reachables(v)))

  add(circuit(x))

  search {
    binaryFirstFail(x)
  } onSolution {
    println(x.map(_.value).mkString(","))
    for (i <- 0 until 8; j <- 0 until 8) {
      val rect = new VisualRectangle(drawing, i * scale, j * scale, scale, scale)
      if (i % 2 == 0 && j % 2 == 0) rect.innerCol = Color.gray
      else if (i % 2 == 1 && j % 2 == 1) rect.innerCol = Color.gray
    }
    for (i <- 0 until 64) {
      val v = x(i).value
      val (c, l) = (v / 8, v % 8)
      new VisualCircle(drawing, scale / 2 + (i / 8) * scale, scale / 2 + (i % 8) * scale, 6).innerCol = Color.RED
      VisualLine(drawing, scale / 2 + (i / 8) * scale, scale / 2 + (i % 8) * scale, scale / 2 + c * scale, scale / 2 + l * scale)
    }
    f.pack()
    drawing.repaint()
  }

  val stats = start(1)

  println(stats)
}
