/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.visual;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.*;

/**
 * @author Pierre Schaus
 * @deprecated(use PlotLine instead,1.0)
 */
public class Plot2D extends JPanel {

	final XYSeries series;
	JFreeChart chart;
	
	public Plot2D(String title, String xlab, String ylab) {
		super(new BorderLayout());

				
		series = new XYSeries("");
		XYDataset xyDataset = new XYSeriesCollection(series);
		chart = ChartFactory.createXYLineChart(title,xlab,ylab,xyDataset,PlotOrientation.VERTICAL,false,false, false);
		chart.getPlot().setBackgroundPaint(Color.white);
		ChartPanel panel = new ChartPanel(chart);
		
		panel.setVisible(true);
		add(panel);
	}
	
	public void addPoint(double x, double y) {
		series.add(x,y);
		chart.fireChartChanged();
	}

    public XYSeries getPoints(){
        return series;
    }


	public static void main(String[] args) {

		VisualFrame f = new VisualFrame("toto");
		JInternalFrame inf = f.createFrame("Drawing");
		final Plot2D demo = new Plot2D("My Plot","xlab","ylab");
		inf.add(demo);
		inf.pack();
		
		
		for (int i = 0; i < 10; i++) {
			demo.addPoint(i, Math.random());
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
	}

}
