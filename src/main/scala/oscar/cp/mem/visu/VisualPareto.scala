package oscar.cp.mem.visu

import oscar.visual._
import java.awt.Color
import java.awt.Dimension
import oscar.cp.mem.pareto.Pareto

class VisualPareto[Sol](val pareto: Pareto[Sol]) extends VisualFrame("Pareto Set Viewer") {
  
  val fWidth = 600
  val fHeight = 600
  setPreferredSize(new Dimension(fWidth, fHeight))
  
  private val xDiffMin = fWidth.toDouble / pareto.nadir(0)
  private val yDiffMin = fHeight.toDouble / pareto.nadir(1)

  private val drawing = new VisualDrawing(false, true)
   
  private val sol = new VisualCircle(drawing, 0, 0, 3, Color.green)  
  private var points : List[VisualCircle] = List()
  
  // Frame
  add(drawing)
  pack

  def update() {       
    points = (for (p <- pareto) yield {      
      val x = p.objs(0)*xDiffMin
      val y = p.objs(1)*yDiffMin     
      new VisualCircle(drawing, x, y, 1, Color.black)
    })
    paint
  }
  
  def paint() {
    drawing.shapes = Array() 
    points.foreach(drawing.addShape(_))      
    drawing.addShape(sol) 
    drawing.repaint()   
  }
  
  def highlight(point: (Int, Int)) {        
    val (xSol, ySol) = point     
    val x = xSol*xDiffMin
    val y = ySol*yDiffMin
    sol.move(x, y)
    paint
  }
}