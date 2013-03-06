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

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * @author Cyrille Dejemeppe
 */
public class ComparativePlot2D extends JPanel {

	JFreeChart chart;
	XYPlot plot;
	XYSeries[] seriesTab;
	int nbSeries;
	
	public ComparativePlot2D(String title, String xlab, String ylab, String[] seriesName, Boolean[] lineVisible, Boolean[] pointVisible) {
		super(new BorderLayout());
		
		nbSeries = seriesName.length;
		seriesTab = new XYSeries[nbSeries];
		for (int i = 0; i < nbSeries; i++) {
			seriesTab[i] = new XYSeries(seriesName[i]);
		}
		
		XYSeriesCollection xyDataset = new XYSeriesCollection();
		for (int i = 0; i < nbSeries; i++) {
			xyDataset.addSeries(seriesTab[i]);
		}
		chart = ChartFactory.createXYLineChart(title,xlab,ylab,xyDataset,PlotOrientation.VERTICAL, true, false, false);
		plot = chart.getXYPlot();
		plot.setBackgroundPaint(Color.white);
		ChartPanel panel = new ChartPanel(chart);
		
		final XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
		for (int i = 0; i < nbSeries; i++) {
			renderer.setSeriesLinesVisible(i, lineVisible[i]);
			renderer.setSeriesShapesVisible(i, pointVisible[i]);
		}
        plot.setRenderer(renderer);
		
		panel.setVisible(true);
		add(panel);
	}
	
	public void addPoint(double x, double y, int seriesIndex) {
		seriesTab[seriesIndex].add(x,y);
		chart.fireChartChanged();
	}

    public static void main(String[] args) {

		VisualFrame f = new VisualFrame("toto");
		JInternalFrame inf = f.createFrame("Drawing");
		final ComparativePlot2D demo = new ComparativePlot2D("My Plot","xlab","ylab", new String[]{"First", "Second", "Third"}, new Boolean[]{true, false, true}, new Boolean[]{true, true, false});
		inf.add(demo);
		inf.pack();
		
		for (int i = 0; i < 10; i++) {
			demo.addPoint(i, Math.random(), 0);
			demo.addPoint(i, Math.random(), 1);
			demo.addPoint(i, Math.random(), 2);
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
	}

}
