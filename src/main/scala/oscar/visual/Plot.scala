/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

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


/**
 * @author Pierre Schaus
 */
abstract class Plot(title: String, xlab: String, ylab: String) extends JPanel(new BorderLayout()) {


	val series: XYSeries = new XYSeries("");
	val xyDataset: XYDataset  = new XYSeriesCollection(series);
	val chart: JFreeChart = createChart()
	chart.getPlot().setBackgroundPaint(Color.white);
	val panel:ChartPanel  = new ChartPanel(chart);
	panel.setVisible(true);
	add(panel);
	
	
	def addPoint(x: Double, y: Double) = {
		series.add(x,y);
		chart.fireChartChanged();
	}
	
	def removeAllPoints() {
		series.clear();
	}

    def getPoints(): XYSeries  = series
    
    def minMax(dom: Range):(Int,Int) = {
      val min = dom.start
      val max = if (dom.isInclusive) dom.end else dom.end-1
      (min,max)
    }
    
    def xDom = chart.getPlot().asInstanceOf[XYPlot].getDomainAxis().getRange()
    
    def yDom = chart.getPlot().asInstanceOf[XYPlot].getRangeAxis().getRange()
    
    
    def xDom_= (dom: Range):Unit = {
      val (min,max) = minMax(dom)
      val xyPlot = chart.getPlot().asInstanceOf[XYPlot];
	  xyPlot.getDomainAxis().setRange(min,max); 
    }
    
    def yDom_= (dom: Range):Unit = {
      val (min,max) = minMax(dom)
      val xyPlot = chart.getPlot().asInstanceOf[XYPlot];
	  xyPlot.getRangeAxis().setRange(min,max); 
    }     
    
    def createChart(): JFreeChart


}

