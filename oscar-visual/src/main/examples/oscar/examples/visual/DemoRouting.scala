/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.examples.visual

import java.awt._
import java.awt.event.{KeyEvent, KeyListener, ActionEvent, ActionListener}
import java.io.IOException
import javax.swing._

import oscar.visual.{VisualBinPacking, VisualDrawing, VisualFrame}
import oscar.visual.map.{Geocoder, Location, VisualMap}
import oscar.visual.plot.PlotLine
import oscar.visual.shapes.VisualRectangle
import oscar.examples.cbls.routing

object DemoRouting {

  val f = new JFrame("The Traveling Salesman Problem")
  def runInThread(p: => Unit) = {
    val thread = new Thread(new Runnable {
      def run = p
    })
    thread.start
  }

  def main(args: Array[String]): Unit = {
  /*
    val tb = f.createToolBar()
    val mb = f.createMenuBar()

    tb.addButton("Inititiate the problem", { runInThread(initiateProblem) })
    tb.addButton("Reset", {runInThread(resetProblem)})
    tb.addButton("Resolve", { runInThread(resolveProblem) })
    f.pack()*/

    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.setSize(1200,800)

    val globalPanel = new JPanel()
    globalPanel.setName("Global Panel")
    globalPanel.setLayout(new BorderLayout())

    val problemValuePanel = new JPanel()
    problemValuePanel.setLayout(new GridBagLayout())
    val gbc = new GridBagConstraints()
    gbc.fill = GridBagConstraints.HORIZONTAL
    gbc.anchor = GridBagConstraints.LINE_START

    //Labels
    val customersLabel = new JLabel("Number of customers : ")
    val warehousesLabel = new JLabel("Number of warehouses : ")


    //TextFields
    val customersTextField = new JTextField()
    customersTextField.setPreferredSize(new Dimension(200,20))
    customersTextField.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        println(customersTextField.getText)
      }
    })
    val warehousesTextField = new JTextField()
    warehousesTextField.setPreferredSize(new Dimension(200,20))
    warehousesTextField.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        println(warehousesTextField.getText)
      }
    })

    def initiateProblem = {
      println(warehousesTextField.getText, customersTextField.getText)
    }

    def resetProblem = {
      warehousesTextField.setText("")
      customersTextField.setText("")
    }

    def resolveProblem = {

    }


    //Buttons
    val initButton = new JButton("Initiate")
    initButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        initiateProblem
      }
    })
    val resetButton = new JButton("Reset")
    resetButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        resetProblem
      }
    })
    val resolveButton = new JButton("Resolve")
    resolveButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        resolveProblem
      }
    })



    gbc.gridx = 0
    gbc.insets = new Insets(10,10,10,10)
    problemValuePanel.add(customersLabel,gbc)
    gbc.gridx = 1
    problemValuePanel.add(customersTextField,gbc)
    gbc.gridx = 2
    problemValuePanel.add(warehousesLabel,gbc)
    gbc.gridx = 3
    problemValuePanel.add(warehousesTextField,gbc)
    gbc.gridx = 4
    problemValuePanel.add(initButton,gbc)
    gbc.gridx = 5
    problemValuePanel.add(resetButton,gbc)
    gbc.gridx = 6
    problemValuePanel.add(resolveButton,gbc)


    globalPanel.add(problemValuePanel, BorderLayout.NORTH)
    f.add(globalPanel)
    f.setVisible(true)




  }
}
