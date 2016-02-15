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
import javax.swing._

import oscar.cbls.invariants.lib.minmax.Max
import oscar.cbls.search.StopWatch
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle}
import oscar.visual.{VisualDrawing, VisualFrame}

import scala.swing.{Dimension, Insets}
import scala.swing.event.MouseEvent

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
  val mapPanel = VisualDrawing(false)

  val objective = f.createFrame("Evolution of the objective function")
  val objGraphic = VisualDrawing(false)

  val jTextFields = new JTextField()::new JTextField()::new JTextField()::new JTextField()::Nil
  val jTextFieldsDefaultValue = "300"::"5"::"10000"::"100000"::Nil
  val jLabels = new JLabel("Among of customers : ")::new JLabel("Among of warehouses : ")::new JLabel("Size of the map : ")::new JLabel("Unrouted penality : ")::Nil

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
      jTextFields(i).addMouseListener(new MouseAdapter {
        def mouseClicked(e:MouseEvent): Unit ={
          println("hello")
          jTextFields(i).setText("")
        }
      })
    }


    map.setLocation(0,0)
    map.setSize(new Dimension(f.getHeight - tb.getHeight - 38,f.getHeight - tb.getHeight - 38))
    map.setLayout(new BorderLayout())
    map.setResizable(false)

    mapPanel.setPreferredSize(new Dimension(map.getHeight,map.getHeight))
    mapPanel.setBorder(BorderFactory.createLineBorder(Color.black,5))

    map.add(mapPanel,BorderLayout.WEST)

    objective.setLocation(map.getWidth,0)
    objective.setSize(new Dimension(f.getWidth - map.getWidth(),map.getHeight))
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
    drawPoints()
    //objValues = controller.myVRP.getObjective().value::objValues
    objValues = 0::objValues
    objTimes = 0::objTimes
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

  def drawMove(routes:scala.List[scala.List[Int]],objInfo:(Int,Long)): Unit ={
    movesCounter += 1
    if(objInfo._1 != Int.MaxValue){
      objValues = objInfo._1::objValues
      objTimes = objInfo._2::objTimes
    }
    if(movesCounter%movesBeforeRepaint == 0) drawRoutes(routes)
  }

  def drawRoutes(routes:scala.List[scala.List[Int]]): Unit ={
    mapPanel.clear()
    drawPoints()
    drawObjectiveCurve()
    println(controller.getWatch)


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
    val graphHeight = objGraphic.getHeight-1*1.0
    objGraphic.clear()
    val time = controller.getWatch*1.0
    val maxObjValue = Math.max(objValues.max*1.0,0)
    val timeUnit = time/graphWidth

    val ordLine = new VisualLine(objGraphic,new Double(0,0,0,graphHeight))
    ordLine.outerCol_$eq(Color.black)
    val absLine = new VisualLine(objGraphic,new Double(0,graphHeight,graphWidth,graphHeight))
    absLine.outerCol_$eq(Color.black)

    var prec:(Long, scala.Double) = null
    if(time < graphWidth){
      for(i <- 0 to objValues.size -1){
        val pos = (objTimes(i),graphHeight-(graphHeight*objValues(i)/maxObjValue))
        if(prec == null){
          new VisualLine(objGraphic,new Double(pos._1,pos._2,pos._1,pos._2))
        }else{
          new VisualLine(objGraphic,new Double(prec._1,prec._2,pos._1,pos._2))
        }
        prec = pos

      }
    }else{
      var currentTimeUnit:Int = 0
      var currentTimeUnitValue:Int = 0
      var currentTimeUnitValuesAmong:Int = 0
      var previousTimeUnitValue:Int = 0
      var previousTimeUnit:Int = 0
      for(i <- objValues.size-2 to 0 by -1){
        if((objTimes(i)/timeUnit).toInt == currentTimeUnit){
          currentTimeUnitValue += (graphHeight*objValues(i)/maxObjValue).toInt
          currentTimeUnitValuesAmong += 1
        }else{
          if(currentTimeUnitValuesAmong != 0) {
            currentTimeUnitValue = currentTimeUnitValue / currentTimeUnitValuesAmong
            new VisualLine(objGraphic, new Double(previousTimeUnit, graphHeight - previousTimeUnitValue, currentTimeUnit, graphHeight - currentTimeUnitValue))
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
      new VisualLine(objGraphic, new Double(previousTimeUnit,graphHeight,previousTimeUnit,0))
    }
  }

  def resetProblem = {
    mapSize = Int.MaxValue
    pointsList = Nil
    colorValues = null
    movesCounter = 0
    objValues = Nil
    mapPanel.clear()
    controller.resetProblem
  }

   def resolveProblem:Unit = {
      if(!controller.resolveProblem)JOptionPane.showMessageDialog(f, "Please first initiate the problem")
      val routesList:scala.List[scala.List[Int]] = (for(c <- 0 to controller.carsAmong-1)yield controller.myVRP.getRouteOfVehicle(c)).toList
      drawRoutes(routesList)
     runInThread(drawObjectiveCurve())
     println(objValues.toString())
   }
}
