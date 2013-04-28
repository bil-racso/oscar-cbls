package oscar.visual

import javax.swing.JToolBar
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

class VisualToolBar(withVisuController: Boolean = false) extends JToolBar {
  
  
  if (withVisuController) {
    import oscar.util.VisualController._
    addButton("Pause", inPause = true)
    addButton("Next", next())
    addButton("Play", play())
  }

  def addButton(label:String,actionOnClick : => Unit){
    val button = new JButton(label)
    button.addActionListener( new ActionListener() {
		override def actionPerformed(e:ActionEvent) { actionOnClick }
	})
	add(button)
	button
  }
}

object VisualToolBar{
  	
  def main(args : Array[String]) {
		val f = new VisualFrame("toto");
		val tb = f.createToolBar(false)
		val d = new VisualDrawing(false);
		val inf = f.createFrame("Drawing");
		inf.add(d);
		f.pack();
		
		tb.addButton("rectangle",{
		  val rect = new VisualRectangle(d, 50, 50, 100, 50);
		  rect.toolTip_$eq("Hello");
		  
		  tb.addButton("make it move",{
			  rect.move(100, 20);
		  })
		})		
	}
}