package oscar.visual

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.StandardChartTheme
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.category.BarRenderer
import org.jfree.data.category.DefaultCategoryDataset

import javax.swing.JPanel
import java.awt.BorderLayout

class BarChart(val title: String, val xLabel: String, val yLabel: String, categories: Array[String], columns: Array[String], val stacked: Boolean) extends JPanel(new BorderLayout()){

  private val dataSet: DefaultCategoryDataset = new DefaultCategoryDataset()
  
  for (cat <- categories; col <- columns) {
    dataSet.addValue(0, cat, col)
  }
  
  ChartFactory.setChartTheme(StandardChartTheme.createLegacyTheme())
  BarRenderer.setDefaultShadowsVisible(false)
  
  val chart = if(stacked) {
    ChartFactory.createStackedBarChart(title, xLabel, yLabel, dataSet, PlotOrientation.VERTICAL, false, false, false)
  }
  else {
    ChartFactory.createBarChart(title, xLabel, yLabel, dataSet, PlotOrientation.VERTICAL, false, false, false)
  }
  
  val plot: CategoryPlot = chart.getPlot().asInstanceOf[CategoryPlot]
  val barRenderer: BarRenderer = plot.getRenderer().asInstanceOf[BarRenderer]
  
  val panel = new ChartPanel(chart)
  panel.setVisible(true)
  add(panel)
  
  def setValue(category: String, column: String, value: Double): Unit = {
    dataSet.setValue(value, category, column)
    chart.fireChartChanged()
  }
  
  def setValue(column: Int, value: Double): Unit = {
    dataSet.setValue(value, "data", ""+column)
    chart.fireChartChanged()
  }
}

object BarChart {
  
  def apply(title: String, xLabel: String, yLabel: String, categories: Array[String], columns: Array[String], stacked: Boolean = false): BarChart = {
    new BarChart(title, xLabel, yLabel, categories, columns, stacked)
  }
  
  def apply(title: String, xLabel: String, yLabel: String, nColumns: Int): BarChart = {
    val categories = Array("data")
    val columns = Array.tabulate(nColumns)(i => i.toString)   
    new BarChart(title, xLabel, yLabel, categories, columns, false)
  }
}

object BarChartExample extends App {
  
  val frame = VisualFrame("Example")
  
  val inFrame1 = frame.createFrame("BarChart 1")
  val barChart1 = BarChart("MyPlot 1", "xLabel", "yLabel", Array("One", "Two"), Array("1", "2"), true)
  barChart1.setValue("One", "1", 1)
  barChart1.setValue("One", "2", 3)
  barChart1.setValue("Two", "2", 2)
  inFrame1.add(barChart1)
  inFrame1.pack()
  
  val inFrame2 = frame.createFrame("BarChart 2")
  val barChart2 = BarChart("MyPlot 2", "xLabel", "yLabel", 3)
  barChart2.setValue(0, 3)
  barChart2.setValue(1, 2)
  inFrame2.add(barChart2)
  inFrame2.pack()
}


