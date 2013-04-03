package oscar.examples.cp.BinPacking.Tester1
import oscar.visual.VisualFrame
import javax.swing.JInternalFrame
import java.awt.Choice
import java.awt.FlowLayout
import java.awt.event.ItemListener
import java.awt.event.ItemEvent
import oscar.visual.Plot2D
import oscar.examples.cp.BinPacking.Tester1.BinPackingResult
import oscar.examples.cp.BinPacking.Tester1.BinPackingResults





object BinPackingResultVisualizer extends App {
	
	println("BinPackingResultVisualizer ")
	val results = new BinPackingResults()
	results.readFile()
	println(results.profilesForVariable("binCapacityDomainSizeMean").map{case (k,v) => k+ " : "+ v}.mkString("\n\n\n\n"))
	
	val f = new VisualFrame("Results");
	val tb = f.createToolBar()
	var frames:List[JInternalFrame] = List()
	
	tb.addButton("+",
	{
	val frame = f.createFrame("")

	
	frames ::= frame
	
	val profileKeyCombo=new Choice();
	val valuesKeyCombo=new Choice();
	val profileCombo=new Choice();
	val graph =	new Plot2D("","","");
	
	frame.add(profileKeyCombo)
	frame.add(valuesKeyCombo)
	frame.add(profileCombo)
	frame.add(graph)
	

	
	
	for(key <- BinPackingResult.PROFILE_KEYS) profileKeyCombo.add(key)
	for(key <- BinPackingResult.VALUES_KEYS) valuesKeyCombo.add(key)
	
	profileKeyCombo.addItemListener(new ItemListener() {
	  def itemStateChanged(event:ItemEvent) {
	    profileCombo.removeAll()
	    for(profile <- results.profilesForVariable(profileKeyCombo.getSelectedItem()).map{case (k,v) => k}) 
	    	profileCombo.add(profile)
	  }
	}) 
	
	tb.addButton("gnuplot files",
	{
	  makeGnuplotFiles()
	  
	})
	
	
	
	
	def updateGraph 
	{
	  graph.clear()
	  val coordinates = results.values(profileKeyCombo.getSelectedItem(), valuesKeyCombo.getSelectedItem(), profileCombo.getSelectedItem())
	  for (coordinate<- coordinates) graph.addPoint(coordinate._1, coordinate._2)
	}
	
	profileKeyCombo.addItemListener(new ItemListener() {
	  def itemStateChanged(event:ItemEvent) {
		  updateGraph
	  }
	})
	
	valuesKeyCombo.addItemListener(new ItemListener() {
	  def itemStateChanged(event:ItemEvent) {
		  updateGraph
	  }
	})
	
	profileCombo.addItemListener(new ItemListener() {
	  def itemStateChanged(event:ItemEvent) {
		  updateGraph
	  }
	})
	 
	
	
	frame.setLayout(new FlowLayout())
	f.pack
	})
	

	f.pack()
	

	
	def makeGnuplotFiles()
	{
	  
	  
	}
}