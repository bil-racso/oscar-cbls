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
package oscar.visual;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.SwingWorker;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.general.PieDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * @author Pierre Schaus
 */
public class BarChart extends JPanel {


	DefaultCategoryDataset dataSet;
	JFreeChart chart;
	String [] categories;
	String [] columns;
	boolean stacked;
	
	public BarChart(String title, String xlab, String ylab, int nbCols) {
		this(title,xlab,ylab,new String[0],new String[0],false);

		for (int i = 0; i < nbCols; i++) {
			dataSet.addValue(0, "data", ""+i);
		}
		
	}
	
	public BarChart(String title, String xlab, String ylab, String [] categories, String [] columns) {
		this(title, xlab, ylab, categories, columns,false);
	}
	
	public BarChart(String title, String xlab, String ylab, String [] categories, String [] columns, boolean stacked) {
		super(new BorderLayout());
		dataSet = new DefaultCategoryDataset();
		this.categories = categories;	
		this.columns = columns;
		for (String c: categories) {
			for (String col: columns) {
				dataSet.addValue(0, c, col);
			}
		}
		if (stacked) {
			chart = ChartFactory.createStackedBarChart(title,xlab,ylab,dataSet,PlotOrientation.VERTICAL,false,false, false);
		}
		else {
			chart = ChartFactory.createBarChart(title,xlab,ylab,dataSet,PlotOrientation.VERTICAL,false,false, false);
		}
			
			
		chart.getPlot().setBackgroundPaint(Color.white);
		ChartPanel panel = new ChartPanel(chart);
		
		panel.setVisible(true);
		add(panel);
	}
	
	public void setValue(String category, String column, double value) {
		dataSet.setValue(value, category, column);
		chart.fireChartChanged();
	}

	public void setValue(int column, double value) {
		dataSet.setValue(value, "data", ""+column);
		chart.fireChartChanged();
	}	

	public static void main(String[] args) {

		VisualFrame f = new VisualFrame("toto");
		
		JInternalFrame inf = f.createFrame("Drawing");
		final BarChart demo = new BarChart("My Plot","xlab","ylab", new String[]{"One","Two"},new String[]{"1","2"},true);
		demo.setValue("One", "1", 1);
		demo.setValue("One", "2", 3);
		demo.setValue("Two", "2", 2);
		inf.add(demo);
		inf.pack();
		
		JInternalFrame inf2 = f.createFrame("Drawing2");
		final BarChart demo2 = new BarChart("My Plot2","xlab","ylab", 3);
		demo2.setValue(0, 3);
		demo2.setValue(1, 2);
		inf2.add(demo2);
		inf2.pack();
		
		
		
		
		
	}

}
