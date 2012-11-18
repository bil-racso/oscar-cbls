package oscar.examples.visual

import oscar.visual.VisualFrame
import oscar.visual.VisualDrawing
import oscar.visual.ColoredShape
import oscar.visual.VisualRectangle
import java.awt.Color
import oscar.visual.VisualBinPacking
import scala.util.Random
import oscar.visual.Plot2D





object DemoVisu {

  val f = new VisualFrame("Results");
  def runInThread(p : => Unit) = {
     val thread = new Thread(new Runnable{
		def run = p
	  })
	  thread.start
  }
  
  def main(args: Array[String]): Unit = {
   
	val tb = f.createToolBar()
	
	tb.addButton("rectangles",{runInThread(demoRectangles)})
	tb.addButton("plot",{runInThread(demoPlot)})	
	tb.addButton("bin bucket",{runInThread(demoBinBucket)})
	tb.addButton("map",{runInThread(demoMap)})	
	f.pack()
    
  }
  
  def demoBinBucket() = {

		val bp = new VisualBinPacking(10, 50);
		
		val inf = f.createFrame("Drawing")
		inf.add(bp)
		f.pack()
		
		val items = Array.tabulate(10)(bp.addItem(_, (Math.random*100).toInt))
		
		val rand = new Random()
		
		
		for (i <- 0 until 100) {
			items(rand.nextInt(items.length)).setBin(rand.nextInt(10))
			Thread.sleep(500)
		}
		
  }
  
  def demoRectangles = {
    	val d = new VisualDrawing(false);
		val inf = f.createFrame("Rectangle");
		inf.add(d);
		f.pack();
		val rect = new VisualRectangle(d, 50, 50, 100, 50);

	
		Thread.sleep(1000);
		rect.innerCol_$eq(Color.red);
		Thread.sleep(1000);
		rect.width = 200;
		Thread.sleep(1000);
		rect.height = 100;
		Thread.sleep(1000);
		rect.move(100, 20);
		for (i <- 0 until 20) {
			Thread.sleep(50);
			rect.move(rect.x+5, rect.y);
		}
		rect.toolTip_$eq("Hello");
  }
  
  def demoPlot = {
	val inf = f.createFrame("Plot");
	val demo = new Plot2D("My Plot","xlab","ylab");
	inf.add(demo);
	inf.pack();
	
	
	for (i <- 0 to 10) {
		demo.addPoint(i, Math.random);
		Thread.sleep(1000);
	}
    
  }
  def demoMap = {
  }
}