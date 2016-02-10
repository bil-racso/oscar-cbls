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

import oscar.cbls.search.StopWatch
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle}
import oscar.visual.{VisualDrawing, VisualFrame}

import scala.swing.{Dimension, Insets}
import scala.swing.event.MouseEvent

object DemoRoutingView extends StopWatch{

  val controller:DemoRoutingController = new DemoRoutingController

  val f = new VisualFrame("The Traveling Salesman Problem")
  val tb = f.createToolBar()
  val param = f.createFrame("Traveling Salesman Problem's Parameters")
  val gbc = new GridBagConstraints()

  var mapSize:Int = Int.MaxValue
  var pointsList:scala.List[(Int, Int)] = Nil
  var colorValues:Array[Color] = null
  var movesCounter:Int = 0
  val movesBeforeRepaint:Int = 10

  /*val mapPanel = new JPanel(){

    var points:Array[(Int,Int)] = new Array[(Int, Int)](0)

    override def paintComponent(g:Graphics): Unit ={
      super.paintComponent(g)
      g.setColor(Color.lightGray)
      g.fillRect(0,0,this.getWidth,this.getHeight)

      g.setColor(Color.BLACK)
      for(p <- points){
        g.fillRect((p._1/mapSize)*getHeight,(p._2/mapSize)*getHeight,2,2)
      }
    }

    def setPoints(p:Array[(Int,Int)]): Unit ={
      points = p
    }
  }*/
  val map = f.createFrame("Traveling Salesman Map")
  val mapPanel = VisualDrawing(false)

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


    tb.addButton("Inititiate the problem", { runInThread(initiateProblem) })
    tb.addButton("Reset", {runInThread(resetProblem)})
    tb.addButton("Resolve", { runInThread(resolveProblem) })


    gbc.fill = GridBagConstraints.NONE
    gbc.anchor = GridBagConstraints.NORTH
    param.setLayout(new GridBagLayout())
    param.setLocation(0,0)
    param.setSize(new Dimension(f.getWidth/5,f.getHeight - tb.getHeight - 38))
    param.setResizable(false)
    param.setBackground(Color.red)


    gbc.insets = new Insets(20,10,10,10)
    param.add(jLabels(0),gbc)
    gbc.gridy = 1
    gbc.insets = new Insets(10,10,10,10)
    param.add(jTextFields(0),gbc)
    gbc.gridy = 2
    gbc.insets = new Insets(10,10,10,10)
    param.add(jLabels(1),gbc)
    gbc.gridy = 3
    param.add(jTextFields(1),gbc)
    gbc.gridy = 4
    gbc.insets = new Insets(10,10,10,10)
    param.add(jLabels(2),gbc)
    gbc.gridy = 5
    gbc.insets = new Insets(10,10,10,10)
    param.add(jTextFields(2),gbc)
    gbc.gridy = 6
    gbc.insets = new Insets(10,10,10,10)
    param.add(jLabels(3),gbc)
    gbc.gridy = 7
    gbc.insets = new Insets(10,10,10,10)
    param.add(jTextFields(3),gbc)

    for(i <- 0 to jTextFields.size-1){
      jTextFields(i).setPreferredSize(new Dimension(jTextFields(i).getParent.getWidth/2,20))
      jTextFields(i).setText(jTextFieldsDefaultValue(i))
      jTextFields(i).addMouseListener(new MouseAdapter {
        @Override
        def mouseClicked(e:MouseEvent): Unit ={
          println("hello")
          jTextFields(i).setText("")
        }
      })
    }

    for(jl <- jLabels){
      jl.setHorizontalAlignment(SwingConstants.LEFT)
    }


    map.setLocation(param.getWidth,0)
    map.setSize(new Dimension((f.getWidth*4/5)-10,f.getHeight - tb.getHeight - 38))
    map.setLayout(new BorderLayout())
    map.setResizable(false)

    mapPanel.setPreferredSize(new Dimension(map.getHeight,map.getHeight))
    mapPanel.setBorder(BorderFactory.createLineBorder(Color.black,5))

    map.add(mapPanel,BorderLayout.WEST)

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
    runInThread(drawPoints())
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

  def drawMove(points:scala.List[scala.List[Int]]): Unit ={
    movesCounter += 1
    if(movesCounter%movesBeforeRepaint == 0) drawRoutes(points)
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

  def resetProblem = {
    mapSize = Int.MaxValue
    pointsList = Nil
    colorValues = null
    movesCounter = 0
    mapPanel.clear()
    controller.resetProblem
  }

   def resolveProblem:Unit = {
      if(!controller.resolveProblem)JOptionPane.showMessageDialog(f, "Please first initiate the problem")
      val routesList:scala.List[scala.List[Int]] = (for(c <- 0 to controller.carsAmong-1)yield controller.myVRP.getRouteOfVehicle(c)).toList
      drawRoutes(routesList)
   }
}
