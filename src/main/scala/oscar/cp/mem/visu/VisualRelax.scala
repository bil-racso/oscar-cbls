package oscar.cp.mem.visu

import oscar.visual._
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Graphics

class VisualRelax(val coord: Array[(Int, Int)], distances: Array[Array[Double]]) extends VisualFrame("Relaxation Viewer") {

  // Some constants
  val nSites = coord.size
  val Sites = 0 until nSites

  val xMargin = 20
  val yMargin = 20

  val selectCol = Color.RED
  val notSelectCol = Color.WHITE
  val normalCol = Color.BLACK

  val maxX = coord.map(_._1).max
  val maxY = coord.map(_._2).max

  val ratio = maxX.toDouble / maxY

  setPreferredSize(new Dimension(400, 400))
  val drawing = new VisualDrawing(false)

  var prev: Array[Int] = Array.tabulate(nSites)(i => i)
  var dist = 0.0
  var nRestart = 1

  // Routes
  val lines = Array.tabulate(nSites)(i => new VisualLine(drawing, 0, 0, 0, 0))

  // Sites
  val circles = Array.tabulate(nSites)(i => {
    val c = new VisualCircle(drawing, 0, 0, 3, normalCol)
    c.innerCol = notSelectCol
    c
  })

  // Distance
  val text: VisualText = new VisualText(drawing, 20, 20, "")
  text.innerCol = normalCol

  // Frame
  add(drawing)
  pack

  override def repaint() {

    val xPanel: Double = drawing.getWidth - xMargin * 2
    val yPanel: Double = drawing.getHeight - yMargin * 2
    
    val panelRatio = xPanel / yPanel

    val scale = if (panelRatio > ratio) yPanel / maxY
    else xPanel / maxX

    val xPadding = xMargin + (xPanel - maxX * scale) / 2
    val yPadding = yMargin + (yPanel - maxY * scale) / 2

    for (i <- Sites) {
      val (x, y) = coord(i)
      val (pX, pY) = coord(prev(i))
      
      circles(i).move(x * scale + xPadding, y * scale + yPadding)
      lines(i).setOrig(x * scale + xPadding, y * scale + yPadding)
      lines(i).setDest(pX * scale + xPadding, pY * scale + yPadding)
    }
    
    text.setText("#Starts: " + nRestart + ", Distance: " + dist)
    drawing.repaint()
  }

  def updateSelected(selected: Array[Boolean]) {
    for (i <- Sites) {
      if (selected(i)) circles(i).innerCol = selectCol
      else circles(i).innerCol = notSelectCol
      if (selected(i) || selected(prev(i))) lines(i).outerCol = selectCol
      else lines(i).outerCol = normalCol
    }
  }

  def updateRoute(newPrev: Array[Int]) {
    prev = newPrev
    repaint()
  }

  def updateDist() {
    dist = sumDistances
    repaint()
  }

  def updateRestart(n: Int) {
    nRestart = n
    repaint()
  }

  def sumDistances = {
    var sum = 0.0
    for (i <- 0 until nSites) sum += distances(i)(prev(i))
    sum
  }
}