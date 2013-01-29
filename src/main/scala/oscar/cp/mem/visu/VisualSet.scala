package oscar.cp.mem.visu

import oscar.visual._
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Graphics
import oscar.cp.mem.pareto.ParetoSet

class VisualSet[Sol](val pareto: ParetoSet[Sol]) extends VisualFrame("Relaxation Viewer") {

  val dim = (600, 600)
  
  val xDiffMin = dim._1.toDouble / pareto.nadir(0)
  val yDiffMin = dim._2.toDouble / pareto.nadir(1)

  val solCol = Color.RED
  val lineCol = new Color(100, 100, 100)

  setPreferredSize(new Dimension(dim._1, dim._2))
  val drawing = new VisualDrawing(false, true)
   
  val sol = new VisualCircle(drawing, 0, 0, 3, Color.green)   
  val lines = Array.fill(4)(new VisualLine(drawing, 0, 0, 0, 0))
  var points : Array[VisualCircle] = Array()

  var ratio = 1
  var xBLCorner = 0
  var yBLCorner = 0
  
  // Frame
  add(drawing)
  pack

  def update() {     
    val xCoeff = ratio*xDiffMin
    val yCoeff = ratio*yDiffMin   
    points = (for (p <- pareto) yield {      
      val x = (p.objs(0)-xBLCorner)*xCoeff
      val y = (p.objs(1)-yBLCorner)*yCoeff      
      new VisualCircle(drawing, x, y, 1, solCol)
    }).toArray
    paint
  }
  
  def paint() {
    drawing.shapes = Array()    
    points.foreach(drawing.addShape(_))        
    drawing.addShape(lines(0))
    drawing.addShape(lines(1))
    drawing.addShape(lines(2))
    drawing.addShape(lines(3))
    drawing.addShape(sol) 
    drawing.repaint()   
  }
  
  def line(bound: Int, obj: Int) = {   
    val xCoeff = ratio*xDiffMin
    val yCoeff = ratio*yDiffMin    
    if (obj == 0 || obj == 2) {
      lines(obj).setDest((bound-xBLCorner)*xCoeff, 0)
      lines(obj).setOrig((bound-xBLCorner)*xCoeff, 600)
    }
    else {     
      lines(obj).setDest(0, (bound-yBLCorner)*yCoeff)
      lines(obj).setOrig(600, (bound-yBLCorner)*yCoeff)
    }
    paint
  }
  
  def selected(point: (Int, Int)) {     
    val xCoeff = ratio*xDiffMin
    val yCoeff = ratio*yDiffMin   
    val (xSol, ySol) = point     
    val x = (xSol-xBLCorner)*xCoeff
    val y = (ySol-yBLCorner)*yCoeff
    sol.move(x, y)
    paint
  }
}