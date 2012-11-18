package oscar.examples.visual

import oscar.visual.VisualFrame
import oscar.visual.VisualDrawing
import oscar.visual.VisualRectangle

object BasicVisu {

  def main(args: Array[String]): Unit = {
   
    
    
    
    
    
    
    
    
    
    
    
    val f = new VisualFrame("Basic");
    val tb = f.createToolBar()
	tb.addButton("foo",println("bar"))
	val d = new VisualDrawing(false);
	val inf = f.createFrame("Rectangle");
	inf.add(d);
	val rect = new VisualRectangle(d, 50, 50, 100, 50);
	rect.toolTip = "Hello";
    
    
  }

}