package oscar.cp.mem.visu

import oscar.visual._
import java.awt.Color
import java.awt.Dimension
import oscar.cp.mem.pareto.Pareto
import org.jdesktop.swingx.decorator.ComponentAdapter
import java.awt.event.ComponentEvent

 class VisualPareto[Sol](val pareto: Pareto[Sol]) extends VisualDrawing(true) {

  val fWidth = 600
  val fHeight = 600
  
  private val sol = new VisualCircle(this, 0, 0, 3, Color.green)  
  private var points : List[VisualCircle] = List()
  

  private val xDiffMin = fWidth.toDouble / pareto.nadir(0)
  private val yDiffMin = fHeight.toDouble / pareto.nadir(1)

  
  def update() {
    println("update")
    points = (for (p <- pareto) yield {      
      val x = p(0)*xDiffMin
      val y = p(1)*yDiffMin     
      new VisualCircle(this, x, y, 1, Color.black)
    })
    paint
  }
  
  def paint() {
    println("paint")
    // remove all
    shapes = Array() 
    points.foreach(addShape(_))      
    addShape(sol) 
    repaint()
  }
  
  
  
  def highlight(point: (Int, Int)) {        
    val (xSol, ySol) = point     
    val x = xSol*xDiffMin
    val y = ySol*yDiffMin
    sol.move(x, y)
    paint
  }
}