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

/**
  * @author fabian.germeau@student.vinci.be
  */
package oscar.examples.cbls.routing

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import oscar.cbls.invariants.core.computation.IntValue
import oscar.cbls.search.StopWatch
import oscar.cbls.visual.FunctionGraphic.{AdjustMaxValue, Zoom}
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.examples.cbls.routing.visual.FunctionGraphic._
import oscar.examples.cbls.routing.visual.MatrixMap.RoutingMatrixVisual
import oscar.visual.VisualFrame

import scala.swing.{Dimension, Insets}

object DemoRoutingView extends StopWatch{
  val controller:DemoRoutingController = new DemoRoutingController

  val f = new JFrame("The Traveling Salesman Problem")
  val tb = new JToolBar()
  val mainPanel = new JPanel()
  val gbc = new GridBagConstraints()

  var mapSize:Int = Int.MaxValue
  var pointsList:scala.List[(Int, Int)] = Nil
  var colorValues:Array[Color] = null

  val routingMap = new RoutingMatrixVisual(pickupAndDeliveryPoints = false)
  val routingMapThread = new Thread(routingMap, "Routing map Thread")

  val objGraph = new ObjFunctionGraphicContainer(dimension = new Dimension(f.getWidth-routingMap.getWidth,360)) with AdjustMaxValue
  val objGraphThread = new Thread(objGraph,"Graphic Thread")

  val result = new JPanel()
  val carsPanel = new JPanel()
  val routesPanel = new JPanel()
  val neighborhoodsPanel = new JPanel()

  val jTextFields = new JTextField()::new JTextField()::new JTextField()::new JTextField()::Nil
  val jTextFieldsDefaultValue = "150"::"5"::"10000"::"100000"::Nil
  val jLabels = new JLabel("Number of customers : ")::new JLabel("Number of warehouses : ")::new JLabel("Size of the map : ")::new JLabel("Unrouted penalty : ")::Nil
  var routesValue:Array[JLabel] = null

  var customersNumber = 0
  var warehouseNumber = 0

  def runInThread(p: => Unit) = {
    val thread = new Thread(new Runnable {
      def run() = p
    })
    thread.start()
  }

  def main(args: Array[String]): Unit = {

    val dim = Toolkit.getDefaultToolkit.getScreenSize
    f.setPreferredSize(new Dimension(dim.getWidth.toInt,(14*dim.getHeight/15).toInt))
    f.setSize(new Dimension(dim.getWidth.toInt,(14*dim.getHeight/15).toInt))
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.setResizable(false)

    mainPanel.setLayout(new BorderLayout())

    tb.setLayout(new FlowLayout(FlowLayout.LEFT,10,1))
    val tempPanelCustomers = new JPanel(new GridBagLayout())
    val tempPanelCars = new JPanel(new GridBagLayout())
    val tempPanelSize = new JPanel(new GridBagLayout())
    val tempPanelPenalty = new JPanel(new GridBagLayout())
    tempPanelCustomers.setPreferredSize(new Dimension(f.getWidth/7,34))
    tempPanelCars.setPreferredSize(new Dimension(f.getWidth/7,34))
    tempPanelSize.setPreferredSize(new Dimension(f.getWidth/7,34))
    tempPanelPenalty.setPreferredSize(new Dimension(f.getWidth/7,34))

    gbc.gridx = 0
    gbc.gridy = 0
    gbc.weightx = 0.0
    tempPanelCustomers.add(new JLabel("Number of customers : "),gbc)
    tempPanelCars.add(new JLabel("Number of cars : "),gbc)
    tempPanelSize.add(new JLabel("Size of the map : "),gbc)
    tempPanelPenalty.add(new JLabel("Unrouted penalty : "),gbc)

    gbc.gridx = 1
    gbc.gridy = 0
    gbc.weightx = 1.0
    gbc.fill = GridBagConstraints.HORIZONTAL
    tempPanelCustomers.add(jTextFields.head,gbc)
    tempPanelCars.add(jTextFields(1),gbc)
    tempPanelSize.add(jTextFields(2),gbc)
    tempPanelPenalty.add(jTextFields(3),gbc)

    tb.add(tempPanelCustomers)
    tb.add(tempPanelCars)
    tb.add(tempPanelSize)
    tb.add(tempPanelPenalty)
    val initButton = new JButton("Initiate the problem")
    initButton.addActionListener(new ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = runInThread(initiateProblem())
    })
    val resetButton = new JButton("Reset")
    resetButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = runInThread(resetProblem())
    })
    val resolveButton = new JButton("Resolve")
    resolveButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = runInThread(resolveProblem())
    })
    tb.add(initButton)
    tb.add(resetButton)
    tb.add(resolveButton)
    mainPanel.add(tb, BorderLayout.NORTH)

    for(i <- jTextFields.indices){
      jTextFields(i).setText(jTextFieldsDefaultValue(i))
      jTextFields(i).setHorizontalAlignment(SwingConstants.LEFT)
    }

    routingMap.setPreferredSize(new Dimension(f.getHeight - tb.getHeight,f.getHeight - tb.getHeight))
    routingMap.setSize(new Dimension(f.getHeight - tb.getHeight,f.getHeight - tb.getHeight))


    mainPanel.add(routingMap,BorderLayout.WEST)

    result.setSize(new Dimension(f.getWidth - routingMap.getWidth, f.getHeight - tb.getHeight))
    result.setLayout(new GridLayout(3,1))

    objGraph.setSize(new Dimension(f.getWidth-routingMap.getWidth,360))
    result.add(objGraph)

    carsPanel.setLayout(new BorderLayout())
    carsPanel.setSize(new Dimension(result.getWidth,(result.getHeight - 360)/2))
    routesPanel.setBackground(Color.white)
    carsPanel.add(new JScrollPane(routesPanel))
    result.add(carsPanel)

    neighborhoodsPanel.setSize(new Dimension(result.getWidth,(result.getHeight - 360)/2))
    neighborhoodsPanel.setBackground(Color.white)
    result.add(new JScrollPane(neighborhoodsPanel))
    mainPanel.add(result, BorderLayout.CENTER)

    f.add(mainPanel)
    f.pack()
    f.setVisible(true)
  }

  def initiateProblem():Unit={
    resetProblem()
    for(jtf <- jTextFields){
      if(!jtf.getText().forall(_.isDigit)){
        JOptionPane.showMessageDialog(f, "Please enter a number in all the fields.")
        return
      }
    }
    mapSize = jTextFields(2).getText().toInt
    val points = controller.initiateProblem(jTextFields.head.getText().toInt,jTextFields(1).getText().toInt,
      jTextFields(2).getText().toInt,jTextFields(3).getText().toInt)

    routesValue = new Array[JLabel](controller.carsNumber)
    for(rv <- routesValue.indices){
      routesValue(rv) = new JLabel("0")
    }

    colorValues = ColorGenerator.generateRandomColors(controller.carsNumber)

    initiateCars()
    initiateMap(mapSize,points)
  }

  def initiateCars() ={
    routesPanel.setLayout(new GridBagLayout)
    for(j <- routesValue.indices){
      val tempPanel = new JPanel()
      tempPanel.setLayout(new GridLayout(1,2))
      val tempLabel = new JLabel("Car number " + (j+1) + " : ")
      tempLabel.setFont(new Font("Serif", Font.BOLD, 16))
      tempLabel.setForeground(colorValues(j))
      tempPanel.add(tempLabel)
      tempPanel.add(routesValue(j))

      gbc.gridx = j%2
      gbc.gridy = j/2
      gbc.anchor = GridBagConstraints.WEST
      gbc.fill = GridBagConstraints.HORIZONTAL
      gbc.insets = new Insets(10,20,10,20)
      routesPanel.add(tempPanel,gbc)
    }
    f.validate()
  }

  /**
    * Initiate the different values needed to draw the map
    * DO NOT switch setMapSize and setPointsList (setPointsList needs the mapSize)
    */
  def initiateMap(mapSize:Int,points:scala.List[(Int,Int)]): Unit ={
    routingMap.setVRP(controller.myVRP)
    routingMap.setColorValues(colorValues)
    routingMap.setMapSize(mapSize)
    routingMap.setPointsList(points)
    routingMap.drawPoints()
    f.validate()
  }

  def drawMove(routes:scala.List[scala.List[Int]],objInfo:(Int,Long,String), hopDistances:Array[IntValue]): Unit ={
    routingMap.setMustRefresh(true,routes)
    objGraph.notifyNewObjectiveValue(objInfo._1,objInfo._2,objInfo._3,ColorGenerator.generateColorFromHash(objInfo._3.hashCode))
    updateRoutes(hopDistances)
  }

  def updateRoutes(hopDistance:Array[IntValue]): Unit ={
    for(h <- hopDistance.indices){
      routesValue(h).setText(hopDistance(h).valueString)
    }
  }

  def displayEndStatistics(stats:String, neighborhoods:scala.List[String]): Unit ={
    neighborhoodsPanel.setLayout(new GridBagLayout)
    val strings = stats.split(" ").filter(_ != "")

    gbc.gridx = 0
    gbc.gridy = 0
    gbc.anchor = GridBagConstraints.WEST
    gbc.fill = GridBagConstraints.HORIZONTAL
    addLine(strings)

    for(n <- neighborhoods.indices){
      val strings = neighborhoods(n).split(" ").filter(_ != "")
      gbc.gridy = n+1
      addLine(strings)
    }

    def addLine(strings:Array[String]): Unit ={
      for(s <- strings.indices){
        if(strings(s) != ""){
          if(s == 0){
            gbc.gridx = 0
            gbc.gridwidth = 5
            neighborhoodsPanel.add(new JLabel(strings(s)),gbc)
          }else{
            gbc.gridx = s+4
            gbc.gridwidth = 1
            neighborhoodsPanel.add(new JLabel(strings(s)),gbc)
          }
        }
      }
    }
    f.validate()

    Thread.sleep(200)
    routingMapThread.interrupt()
    objGraphThread.interrupt()
  }

  def resetProblem() = {
    mapSize = Int.MaxValue
    pointsList = Nil
    routingMap.clear()
    objGraph.clear()
    controller.resetProblem
    routesPanel.removeAll()
    routesValue = null
    neighborhoodsPanel.removeAll()
  }

  def resolveProblem() = {
    objGraphThread.start()
    routingMapThread.start()
    if(!controller.resolveProblem)JOptionPane.showMessageDialog(f, "Please first initiate the problem")
  }

}
