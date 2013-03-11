/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/


package oscar.visual

import javax.swing.JPanel
import java.awt.BorderLayout
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYDataset
import org.jfree.chart.JFreeChart
import org.jfree.chart.ChartPanel
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.ChartFactory
import java.awt.Color
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.plot.CombinedDomainXYPlot
import org.jfree.chart.plot.ValueMarker
import org.jfree.ui.RectangleAnchor
import org.jfree.ui.TextAnchor


/**
 * @author Pierre Schaus
 */
class PlotScatter(title: String, xlab: String, ylab: String) extends Plot(title,xlab,ylab) {
  
	def createChart = ChartFactory.createScatterPlot(title,xlab,ylab,xyDataset,PlotOrientation.VERTICAL,false,false, false);


}

object PlotScatter extends App {
		val f = new VisualFrame("toto");
		val inf = f.createFrame("Drawing");
		val myPlot = new PlotScatter("My Scatter Plot","xlab","ylab");
		inf.add(myPlot);
		inf.pack();
		
				
		myPlot.xDom = 0 to 10
		myPlot.yDom = 0 to 1
		
		for (i <- 0 until 10) {
		    val y = Math.random()
			myPlot.addPoint(i, y);
			myPlot.highlight(i,y, Color.green)
			Thread.sleep(1000);

		} 
		
		
		
}
