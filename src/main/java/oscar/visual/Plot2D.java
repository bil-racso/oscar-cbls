/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.general.PieDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * @author Pierre Schaus
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
