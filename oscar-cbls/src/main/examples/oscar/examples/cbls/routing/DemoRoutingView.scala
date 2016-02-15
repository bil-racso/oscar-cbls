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
package oscar.examples.cbls.routing

import java.awt._
import java.awt.event.{MouseAdapter}
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D
import javax.swing._
import javax.swing.border.EmptyBorder

import oscar.cbls.invariants.lib.minmax.Max
import oscar.cbls.search.StopWatch
import oscar.cbls.search.core.Neighborhood
import oscar.visual.shapes.{VisualText, VisualCircle, VisualLine, VisualRectangle}
import oscar.visual.{VisualDrawing, VisualFrame}

import scala.swing.{Dimension, Insets}
import scala.swing.event.{FontChanged, MouseEvent}

object DemoRoutingView extends StopWatch{



  val controller:DemoRoutingController = new DemoRoutingController

  val f = new VisualFrame("The Traveling Salesman Problem")
  val tb = f.createToolBar()
  //val param = f.createFrame("Traveling Salesman Problem's Parameters")
  val gbc = new GridBagConstraints()

  var mapSize:Int = Int.MaxValue
  var pointsList:scala.List[(Int, Int)] = Nil
  var colorValues:Array[Color] = null
  var movesCounter:Int = 0
  var objValues:scala.List[Int] = Nil
  var objTimes:scala.List[Long] = Nil
  val movesBeforeRepaint:Int = 10

  val map = f.createFrame("Traveling Salesman Map")
  val mapPanel = new VisualDrawing(false, false)

  val objective = f.createFrame("Evolution of the objective function")
  val objGraphic = new VisualDrawing(false, false)

  val result = f.createFrame("Results of the routing")
  val carsPanel = new JPanel()
  val routesPanel = new JPanel()
  val neighborhoodsPanel = new JPanel()

  val jTextFields = new JTextField()::new JTextField()::new JTextField()::new JTextField()::Nil
  val jTextFieldsDefaultValue = "300"::"5"::"10000"::"100000"::Nil
  val jLabels = new JLabel("Among of customers : ")::new JLabel("Among of warehouses : ")::new JLabel("Size of the map : ")::new JLabel("Unrouted penality : ")::Nil
  var routesValue:Array[JLabel] = null

  var customersAmong = 0
  var warehouseAmong = 0

  def runInThread(p: => Unit) = {
    val thread = new Thread(new Runnable {
      def run = p
    })
    thread.start
  }

  def main(args: Array[String]): Unit = {

    f.setMinimumSize(f.getSize)
    f.setResizable(false)


    tb.setLayout(new FlowLayout(FlowLayout.LEFT,10,1))
    val tempPanelCustomers = new JPanel(new GridBagLayout())
    val tempPanelCars = new JPanel(new GridBagLayout())
    val tempPanelSize = new JPanel(new GridBagLayout())
    val tempPanelPenality = new JPanel(new GridBagLayout())
    tempPanelCustomers.setPreferredSize(new Dimension(tb.getWidth/7,26))
    tempPanelCars.setPreferredSize(new Dimension(tb.getWidth/7,26))
    tempPanelSize.setPreferredSize(new Dimension(tb.getWidth/7,26))
    tempPanelPenality.setPreferredSize(new Dimension(tb.getWidth/7,26))

    gbc.gridx = 0
    gbc.gridy = 0
    gbc.weightx = 0.0
    tempPanelCustomers.add(new JLabel("Among of customers : "),gbc)
    tempPanelCars.add(new JLabel("Among of cars : "),gbc)
    tempPanelSize.add(new JLabel("Size of the map : "),gbc)
    tempPanelPenality.add(new JLabel("Unrouted penality : "),gbc)

    gbc.gridx = 1
    gbc.gridy = 0
    gbc.weightx = 1.0
    gbc.fill = GridBagConstraints.HORIZONTAL
    tempPanelCustomers.add(jTextFields(0),gbc)
    tempPanelCars.add(jTextFields(1),gbc)
    tempPanelSize.add(jTextFields(2),gbc)
    tempPanelPenality.add(jTextFields(3),gbc)

    tb.add(tempPanelCustomers)
    tb.add(tempPanelCars)
    tb.add(tempPanelSize)
    tb.add(tempPanelPenality)
    tb.addButton("Inititiate the problem", { runInThread(initiateProblem) })
    tb.addButton("Reset", {runInThread(resetProblem)})
    tb.addButton("Resolve", { runInThread(resolveProblem) })

    for(i <- 0 to jTextFields.size-1){
      jTextFields(i).setText(jTextFieldsDefaultValue(i))
      jTextFields(i).setHorizontalAlignment(SwingConstants.LEFT)
    }


    map.setLocation(0,0)
    map.setSize(new Dimension(f.getHeight - tb.getHeight - 38,f.getHeight - tb.getHeight - 38))
    map.setLayout(new BorderLayout())
    map.setResizable(false)

    mapPanel.setPreferredSize(new Dimension(map.getHeight,map.getHeight))
    mapPanel.setBorder(BorderFactory.createLineBorder(Color.black,5))

    map.add(mapPanel,BorderLayout.WEST)

    objective.setLocation(map.getWidth,0)
    objective.setResizable(false)
    objective.setSize(new Dimension(f.getWidth - map.getWidth(),220))
    objective.setLayout(new GridBagLayout())
    gbc.gridx = 0
    gbc.gridy = 0
    gbc.insets = new Insets(5,20,5,5)
    gbc.anchor = GridBagConstraints.WEST
    gbc.fill = GridBagConstraints.HORIZONTAL
    gbc.weighty = 0.0
    objective.add(new JLabel("Evolution of the objective function over time"),gbc)

    objGraphic.setPreferredSize(new Dimension(objective.getWidth,objective.getHeight-20))
    objGraphic.setBorder(BorderFactory.createLineBorder(Color.black,5))

    gbc.gridx = 0
    gbc.gridy = 1
    gbc.insets = new Insets(0,5,5,5)
    gbc.anchor = GridBagConstraints.CENTER
    gbc.fill = GridBagConstraints.BOTH
    gbc.weighty = 1.0
    objective.add(objGraphic,gbc)

    result.setLocation(map.getWidth,objective.getHeight)
    result.setSize(new Dimension(f.getWidth - map.getWidth, f.getHeight - objective.getHeight - tb.getHeight - 38))
    result.setLayout(new GridBagLayout)
    result.setResizable(false)

    carsPanel.setLayout(new BorderLayout)
    carsPanel.add(new JScrollPane(routesPanel))
    carsPanel.setMaximumSize(new Dimension(result.getWidth,result.getHeight/2))
    gbc.gridx = 0
    gbc.gridy = 0
    gbc.insets = new Insets(5,5,5,5)
    gbc.anchor = GridBagConstraints.WEST
    gbc.fill = GridBagConstraints.BOTH
    result.add(carsPanel,gbc)

    neighborhoodsPanel.setMaximumSize(new Dimension(result.getWidth,result.getHeight/2))
    gbc.gridy = 1
    result.add(new JScrollPane(neighborhoodsPanel),gbc)

    f.pack()

  }

  def initiateProblem():Unit={
    resetProblem
    for(jtf <- jTextFields){
      if(!jtf.getText().forall(_.isDigit)){
        JOptionPane.showMessageDialog(f, "Please enter a number in all the fields.")
        return
      }
    }
    mapSize = jTextFields(2).getText().toInt
    val points = controller.initiateProblem(jTextFields(0).getText().toInt,jTextFields(1).getText().toInt,
      jTextFields(2).getText().toInt,jTextFields(3).getText().toInt)

    for(p <- points.reverse){
      val tempP = (p._1*mapPanel.getHeight/mapSize, p._2*mapPanel.getHeight/mapSize)
      pointsList = tempP::pointsList
    }

    colorValues = new Array[Color](controller.carsAmong)
    for(c <- 0 to colorValues.length-1){
      colorValues(c) = new Color((255*Math.random()).toInt,(255*Math.random()).toInt,(255*Math.random()).toInt)
    }

    routesValue = new Array[JLabel](controller.carsAmong)
    for(rv <- 0 to routesValue.length-1){
      routesValue(rv) = new JLabel("0")
    }

    initiateCars()
    drawPoints()
  }

  def initiateCars(): Unit ={
    routesPanel.setLayout(new GridLayout(controller.carsAmong/2,2))
    for(j <- 0 to routesValue.length -1){
      val tempPanel = new JPanel()
      tempPanel.setBorder(new EmptyBorder(5,5,5,5))
      tempPanel.setLayout(new GridLayout(1,2))
      val tempLabel = new JLabel("Car number " + (j+1) + " : ")
      tempLabel.setFont(new Font("Serif", Font.BOLD, 16))
      tempLabel.setForeground(colorValues(j))
      tempPanel.add(tempLabel)
      tempPanel.add(routesValue(j))

      routesPanel.add(tempPanel,gbc)
    }
    f.validate()
  }

  def drawMove(routes:scala.List[scala.List[Int]],objInfo:(Int,Long)): Unit ={
    movesCounter += 1
    objValues = objInfo._1::objValues
    objTimes = objInfo._2::objTimes

    if(movesCounter%movesBeforeRepaint == 0)drawRoutes(routes)
    drawObjectiveCurve()
    updateRoutes(routes)
  }

  def drawPoints(): Unit ={
    for(p <- pointsList){
      if(pointsList.indexOf(p) < controller.carsAmong){
        val tempPoint = new VisualCircle(mapPanel,p._1.toInt,p._2.toInt,5)
        tempPoint.innerCol_$eq(colorValues(pointsList.indexOf(p)))
      }
      else{
        val tempPoint = new VisualCircle(mapPanel,p._1.toInt,p._2.toInt,2)
        tempPoint.innerCol_$eq(Color.black)
      }
    }
  }

  def drawRoutes(routes:scala.List[scala.List[Int]]): Unit ={
    mapPanel.clear()
    drawPoints()

    for(r <- 0 to controller.carsAmong-1){
      val color:Color = colorValues(r)
      val points = routes(r)
      var old = points(0)
      for(p <- points){
        val tempRoute = new VisualLine(mapPanel,new Double(pointsList(old)._1, pointsList(old)._2,pointsList(p)._1,pointsList(p)._2))
        tempRoute.outerCol_$eq(color)
        old = p
      }
      val tempRoute = new VisualLine(mapPanel,new Double(pointsList(old)._1, pointsList(old)._2,pointsList(points(0))._1,pointsList(points(0))._2))
      tempRoute.outerCol_$eq(color)
    }
  }

  def drawObjectiveCurve() = {
    val graphWidth = objGraphic.getWidth*1.0
    val graphHeight = objGraphic.getHeight-1*1.0 - 10
    objGraphic.clear()
    val time = controller.getWatch*1.0
    val timeUnit = time/graphWidth
    val maxObjValue = objValues.max

    val minObjValue = objValues.min

    def getFloorValues(): (Int,Int) ={
      var minFloor = 1
      var maxFloor = 1
      while(maxObjValue/maxFloor > 10){
        if(minObjValue/minFloor > 10)
          minFloor *=10
        maxFloor *= 10
      }
      (minFloor,maxFloor)
    }
    val floorValues = getFloorValues

    drawAxes(70,0,graphWidth.toInt,(graphHeight).toInt,floorValues._1,floorValues._2)
    var prec:(Long, scala.Double) = null
    if(time < graphWidth - 70){
      for(i <- 0 to objValues.size -1){
        val pos = (objTimes(i)+70,graphHeight-(graphHeight*objValues(i)/maxObjValue))
        if(prec == null){
          new VisualLine(objGraphic,new Double(pos._1,pos._2,pos._1,pos._2))
        }else{
          new VisualLine(objGraphic,new Double(prec._1,prec._2,pos._1,pos._2))
        }
        prec = pos

      }
    }else{
      var currentTimeUnit:Int = 0
      var currentTimeUnitValue:scala.Double = 0.0
      var currentTimeUnitValuesAmong:scala.Double = 0.0
      var previousTimeUnitValue:scala.Double = 0.0
      var previousTimeUnit:scala.Double = 0.0
      for(i <- objValues.size-1 to 0 by -1){
        if((objTimes(i)/timeUnit).toInt == currentTimeUnit){
          currentTimeUnitValue += objValues(i)
          currentTimeUnitValuesAmong += 1
        }else{
          if(currentTimeUnitValuesAmong != 0) {
            currentTimeUnitValue = (((graphHeight) * Math.log((currentTimeUnitValue / currentTimeUnitValuesAmong) / floorValues._1))/Math.log(maxObjValue/floorValues._1)).toInt
            new VisualLine(objGraphic, new Double(previousTimeUnit+70, graphHeight - previousTimeUnitValue, currentTimeUnit+70, graphHeight - currentTimeUnitValue))
            previousTimeUnit = currentTimeUnit
            previousTimeUnitValue = currentTimeUnitValue
            currentTimeUnitValue = 0
            currentTimeUnitValuesAmong = 0
          }
          while(currentTimeUnit != (objTimes(i)/timeUnit).toInt){
            currentTimeUnit += 1
          }
        }
      }
    }
  }

  def drawAxes(sW:Int,sH:Int,eW:Int,eH:Int,minF:Int,maxF:Int): Unit ={
    val ordLine = new VisualLine(objGraphic,new Double(sW,0,sW,eH))
    ordLine.outerCol_$eq(Color.black)
    val absLine = new VisualLine(objGraphic,new Double(sW,eH,eW,eH))
    absLine.outerCol_$eq(Color.black)

    var f = maxF
    var i = 0
    while(f >= minF){
      f /= 10
      i += 1
    }
    var y = i
    while(f <= maxF){
      f *= 10
      new VisualText(objGraphic,0,y*(eH/i),f.toString,false,new Rectangle2D.Double(0, 0, 1, 1))
      y -= 1
    }
  }

  def updateRoutes(routes:scala.List[scala.List[Int]]): Unit ={
    for(r <- 0 to routes.length-1){
      val tempVal = routes(r).foldLeft(0)((a,b)=>a+b)
      routesValue(r).setText(tempVal.toString)
    }
  }

  def displayEndStatistics(stats:String, neighborhoods:scala.List[String]): Unit ={
    neighborhoodsPanel.setLayout(new GridBagLayout)
    val strings = stats.split(" ").filter(_ != "")

    gbc.gridx = 0
    gbc.gridy = 0
    gbc.anchor = GridBagConstraints.WEST
    gbc.fill = GridBagConstraints.HORIZONTAL
    gbc.weightx = 0.0
    for(s <- 0 to strings.length-1){
      if(strings(s) != ""){
        if(s == 0){
          gbc.gridwidth = 5
          gbc.gridx = 0
          neighborhoodsPanel.add(new JLabel(strings(s)),gbc)
        }else{
          gbc.gridx = s+4
          gbc.gridwidth = 1
          neighborhoodsPanel.add(new JLabel(strings(s)),gbc)
        }
      }
    }
    for(n <- 0 to neighborhoods.length-1){
      val strings = neighborhoods(n).split(" ").filter(_ != "")
      gbc.gridy = n+1
      for(s <- 0 to strings.length-1){
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
  }

  def resetProblem = {
    mapSize = Int.MaxValue
    pointsList = Nil
    colorValues = null
    movesCounter = 0
    objValues = Nil
    mapPanel.clear()
    objGraphic.clear()
    controller.resetProblem
    routesPanel.removeAll()
  }

  def resolveProblem:Unit = {
    if(!controller.resolveProblem)JOptionPane.showMessageDialog(f, "Please first initiate the problem")
    val routesList:scala.List[scala.List[Int]] = (for(c <- 0 to controller.carsAmong-1)yield controller.myVRP.getRouteOfVehicle(c)).toList
    drawRoutes(routesList)
    drawObjectiveCurve()
  }

}
