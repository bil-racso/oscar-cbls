package oscar.examples.cbls.routing.visual.ObjFunctionCurve

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
import java.awt._
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D

import oscar.cbls.search.StopWatch
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualLine, VisualText}

import scala.collection.mutable.ListBuffer

/**
  * @author fabian.germeau@student.vinci.be
  */

abstract class ObjFunctionGraphic extends VisualDrawing(false,false) with StopWatch{

  val minWidth = 70
  val minHeight = 10
  var maxHeight = getHeight-30
  var maxWidth = getWidth - 10
  val objValues:ListBuffer[Int] = new ListBuffer[Int]
  val objTimes:ListBuffer[Long] = new ListBuffer[Long]
  val objBestValues:ListBuffer[Int] = new ListBuffer[Int]
  var minObjTime:Long = 0
  var maxObjTime:Long = 0
  var maxNumberOfObj:Int = 0
  var best = Int.MaxValue

  setLayout(new BorderLayout())

  def notifyNewObjectiveValue(objValue:Int, objTime:Long)

  def clear(): Unit ={
    super.clear()
  }

  def drawGlobalCurve()

  def drawObjectiveCurve()

  def setTimeBorders(position:Int){}

  def setMaxNumberOfObject(percentage:scala.Double){}
}

class ObjFunctionGraphicImpl extends ObjFunctionGraphic{


  override def clear(): Unit ={
    super.clear()
    objValues.clear()
    objTimes.clear()
    objBestValues.clear()
    best = Int.MaxValue
  }

  //Save the objectif value and the time value of the current state
  def notifyNewObjectiveValue(objValue:Int, time:Long): Unit ={
    maxHeight = getHeight - 30
    maxWidth = getWidth - 10
    objTimes.append(time)
    objValues.append(objValue)
    best = Math.min(best,objValue)
    objBestValues.append(best)
    maxObjTime = time
    maxNumberOfObj = objValues.length
  }

  //Prepare the different values needed to draw the curve
  def drawObjectiveCurve() = {
    super.clear()

    val usedLists = getUsedLists

    val maxObjValue = usedLists._1.max
    val minObjValue = Math.min(usedLists._1.min,best)
    val diffObjValue = maxObjValue - minObjValue

    val maxTimeValue = usedLists._2.max
    val minTimeValue = usedLists._2.min
    val diffTimeValue = maxTimeValue - minTimeValue

    val ordFloorValues = getOrdFloorValues(diffObjValue,minObjValue,maxObjValue)
    val absFloorValues = getAbsFloorValues(diffTimeValue,minTimeValue,maxTimeValue)

    drawAxis(ordFloorValues._1,ordFloorValues._2,absFloorValues._1,absFloorValues._2)
    drawCurve(usedLists._1,usedLists._2,usedLists._3,ordFloorValues._1,ordFloorValues._2,maxObjValue)
  }

  //Prepare the different values needed to draw the global curve
  def drawGlobalCurve() = {
    super.clear()
    maxNumberOfObj = objValues.length
    val maxObjValue = objValues.max
    val minObjValue = objValues.min
    val diffObjValue = maxObjValue - minObjValue

    val maxTimeValue = objTimes.max
    val minTimeValue = objTimes.min
    val diffTimeValue = maxTimeValue - minTimeValue

    val ordFloorValues = getOrdFloorValues(diffObjValue,minObjValue,maxObjValue)
    val absFloorValues = getAbsFloorValues(diffTimeValue,minTimeValue,maxTimeValue)

    drawAxis(ordFloorValues._1,ordFloorValues._2,absFloorValues._1,absFloorValues._2)
    drawCurve(ordFloorValues._1,ordFloorValues._2,maxObjValue)
  }

  //Draw the global curve, including all the values
  def drawCurve(minOrdValue:Int,maxOrdValue:Int,maxObjValue:Int): Unit = {
    val timeUnit:scala.Double = Math.max(maxObjTime/(maxWidth-minWidth),1.0)
    var currentTimeUnit:Int = 0
    var currentTimeUnitValue:scala.Double = 0.0
    var currentTimeUnitValuesNumber:scala.Double = 0.0
    var previousTimeUnitValue:scala.Double = 0.0
    var previousTimeUnit:scala.Double = 0.0
    for(i <- objValues.indices){
      if((objTimes(i)/timeUnit).toInt == currentTimeUnit){
        currentTimeUnitValue += objValues(i)
        currentTimeUnitValuesNumber += 1
      }else{
        if(currentTimeUnitValuesNumber != 0) {
          currentTimeUnitValue = adjustHeight(currentTimeUnitValue/currentTimeUnitValuesNumber,minOrdValue,maxOrdValue)
          if(previousTimeUnitValue == 0)
            previousTimeUnitValue = currentTimeUnitValue
          new VisualLine(this, new Double(previousTimeUnit+70, previousTimeUnitValue, currentTimeUnit+70, currentTimeUnitValue))
          previousTimeUnit = currentTimeUnit
          previousTimeUnitValue = currentTimeUnitValue
          currentTimeUnitValue = 0
          currentTimeUnitValuesNumber = 0
        }
        while(currentTimeUnit != (objTimes(i)/timeUnit).toInt){
          currentTimeUnit += 1
        }
      }
    }
    new VisualLine(this,new Double(previousTimeUnit+70, previousTimeUnitValue, maxWidth, previousTimeUnitValue))
  }

  //Draw a part of the curve, the displayed values are defined whit the getUsedList method
  def drawCurve(usedObjValues:scala.List[Int], usedObjTimes:scala.List[Long], usedObjBestValues:scala.List[Int],minOrdValue:Int,maxOrdValue:Int,maxObjValue:Int): Unit ={
    var prev:(Int, Int) = null
    var prevBest:Int = 0
    for(i <- usedObjValues.indices) {
      val x = adjustWidth(usedObjTimes(i),minObjTime,maxObjTime)
      val y = adjustHeight(usedObjValues(i), minOrdValue, maxOrdValue)
      val pos = (x,y)
      val yBest = adjustHeight(usedObjBestValues(i), minOrdValue, maxOrdValue)
      if(prev != null){
        val realLine = new VisualLine(this,new Double(prev._1,prev._2,pos._1,pos._2))
        realLine.outerCol_$eq(Color.black)
        val bestLine = new VisualLine(this,new Double(prev._1,prevBest,pos._1,yBest))
        bestLine.outerCol_$eq(Color.green)
      }
      prev = pos
      prevBest = yBest
    }
  }

  //Adjust the value of an objectif to the size of the window
  def adjustHeight(value:scala.Double, minOrdValue:Int, maxOrdValue:Int): Int ={
    (maxHeight - ((maxHeight-minHeight) * (value - minOrdValue)/Math.max(maxOrdValue - minOrdValue,1))).toInt
  }

  //Adjust the time of occurrence of an objectif to the size of the window
  def adjustWidth(value:scala.Double, minAbsValue:scala.Double, maxAbsValue:scala.Double): Int ={
    (minWidth + (maxWidth - minWidth) * (value - minAbsValue)/Math.max(maxAbsValue - minAbsValue,1)).toInt
  }

  //Return the value of the Y axis's border
  def getOrdFloorValues(diffObjValue:Int, minObjValue:Int, maxObjValue:Int, diffFloor:Int = 1): (Int,Int) ={
    if(diffFloor <= diffObjValue){
      getOrdFloorValues(diffObjValue,minObjValue,maxObjValue,diffFloor*10)
    }else{
      var minFloor = 1
      while(minObjValue/minFloor > 0){
        minFloor *= 10
      }
      val df = Math.max(diffFloor / 10, 1)
      (Math.max((minObjValue / df) * df, (minObjValue / minFloor) * minFloor), ((maxObjValue / df) * df) + df)
    }
  }

  //Return the value of the X axis's border
  def getAbsFloorValues(diffTimeValue:scala.Double, minTimeValue:scala.Double, maxTimeValue:scala.Double, diffFloor:scala.Double = 1): (scala.Double, scala.Double) ={
    if(diffFloor <= diffTimeValue){
      getAbsFloorValues(diffTimeValue,minTimeValue,maxTimeValue,diffFloor*10)
    }else{
      var minFloor = 1
      while(minTimeValue/minFloor > 0){
        minFloor *= 10
      }
      val df = Math.max(diffFloor / 10, 1)
      (Math.max((minTimeValue / df) * df, (minTimeValue / minFloor) * minFloor), Math.min(((maxTimeValue / df) * df) + df,maxObjTime))
    }
  }

  //Return the list of elements that will be considered for the drawing of the curve
  def getUsedLists: (scala.List[Int], scala.List[Long], scala.List[Int]) = {
    val usedObjValues: ListBuffer[Int] = new ListBuffer[Int]
    val usedObjTimes: ListBuffer[Long] = new ListBuffer[Long]
    val usedObjBestValues: ListBuffer[Int] = new ListBuffer[Int]

    //To avoid the problem when you have no value in the specified time range
    for(i <- objValues.length - maxNumberOfObj until objValues.length){
      usedObjValues.append(objValues(i))
      usedObjTimes.append(objTimes(i))
      usedObjBestValues.append(objBestValues(i))
    }

    val tempObjTimesList = usedObjTimes.toList
    var removedElems = 0
    for(i <- tempObjTimesList.indices){
      if(!(tempObjTimesList(i) > minObjTime && tempObjTimesList(i) < maxObjTime)) {
        usedObjValues.remove(i-removedElems)
        usedObjTimes.remove(i-removedElems)
        usedObjBestValues.remove(i-removedElems)
        removedElems += 1
      }
    }
    //To fill the blank when there is no registred value between the minObjTime and the maxObjTime
    if(usedObjValues.isEmpty){
      usedObjValues.append(objValues(objTimes.lastIndexWhere(_<minObjTime)))
      usedObjTimes.append(minObjTime)
      usedObjBestValues.append(objBestValues(objTimes.lastIndexWhere(_<minObjTime)))
      usedObjValues.append(objValues(objTimes.indexWhere(_>maxObjTime)))
      usedObjTimes.append(maxObjTime)
      usedObjBestValues.append(objBestValues(objTimes.indexWhere(_>maxObjTime)))
    }else {
      //To fill the blank at the end of the currently drawn curve
      usedObjValues.append(usedObjValues.last)
      usedObjTimes.append(maxObjTime)
      usedObjBestValues.append(usedObjBestValues.last)
      //To fill the blank at the beginning of the currently drawn curve
      usedObjValues.insert(0,usedObjValues.head)
      usedObjTimes.insert(0,minObjTime)
      usedObjBestValues.insert(0,usedObjBestValues.head)
    }
    (usedObjValues.toList,usedObjTimes.toList,usedObjBestValues.toList)
  }

  //Draw the axis
  def drawAxis(minF:Int,maxF:Int, minT:scala.Double, maxT:scala.Double): Unit ={
    val ordLine = new VisualLine(this,new Double(minWidth,minHeight,minWidth,maxHeight))
    ordLine.outerCol_$eq(Color.black)
    val absLine = new VisualLine(this,new Double(minWidth,maxHeight,maxWidth,maxHeight))
    absLine.outerCol_$eq(Color.black)

    val objStep = (maxF - minF)/10
    for(i <- 1 to 10){
      val scaleHeight = adjustHeight(i*objStep + minF,minF,maxF)
      new VisualText(this,5,scaleHeight,(minF+objStep*i).toString,false,new Rectangle2D.Double(0, 0, 1, 1))
      new VisualLine(this,new Double(minWidth-10,scaleHeight,minWidth,scaleHeight))
    }
    val timeStep = (maxT - minT)/10
    for(i <- 1 to 10){
      val scaleWidth = adjustWidth(i*timeStep + minT,minT,maxT)
      new VisualText(this,scaleWidth,maxHeight+20,((timeStep*i)+minT).toInt.toString,true,new Rectangle2D.Double(0, 0, 1, 1))
      new VisualLine(this,new Double(scaleWidth,maxHeight,scaleWidth,maxHeight+10))
    }
  }
}
