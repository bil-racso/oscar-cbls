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
package oscar.cbls.scheduling

import java.awt.Color
import oscar.visual.{VisualLine, VisualText, VisualRectangle, VisualDrawing, VisualUtil}

class Gantt(p:Planning) extends VisualDrawing(false) {

  var LineCount = 0
  val LineArray:Array[Int] = makeLineArray(p)
  val colors:Array[Color] = Array.tabulate(p.ActivityArray.size)(_ => VisualUtil.getRandomColor())
  def makeLineArray(p:Planning):Array[Int] = {
    var currentline = -1
    var Front:List[Int] = List.empty
    val GantLines:Array[Int] = Array.tabulate(p.ActivityArray.size)((i:Int)
    => (if (p.ActivityArray(i).StaticPredecessors.isEmpty){
      currentline +=1
      Front = i :: Front
      currentline
    }else{
      -1
     }))
    LineCount = currentline -1
    def decorate(startid:Int){
      val task:Activity = p.ActivityArray(startid)
      for (t <- task.AllSucceedingActivities.value){
        val succeedingTask:Activity = p.ActivityArray(t)
        if (GantLines(succeedingTask.ID) == -1){
          GantLines(succeedingTask.ID) = GantLines(task.ID)
          decorate(t)
        }
      }
    }

    for (startid <- Front) decorate(startid)
    GantLines
  }
  
  private val rectangles : Array[VisualRectangle] = Array.tabulate(p.ActivityArray.size)(a => {
    val rect = new VisualRectangle(this, 0, 0, 0, 0)
    rect.innerCol = colors(a)
    rect.toolTip = p.ActivityArray(a).name
    rect
  })

  private val text : VisualText = new VisualText(this, 50, 50, "",true)
  text.innerCol = Color.RED

  private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
  makespanLine.outerCol = Color.RED;

  def update(xScale : Float, yScale: Int) {

    for (task <- p.Activities) {

      val i = task.ID
      rectangles(i).width = ((task.duration.value)*xScale).ceil
      rectangles(i).height = yScale
      rectangles(i).innerCol = colors(i)
      rectangles(i).move((task.EarliestStartDate.value*xScale).ceil, LineArray(task.ID)*yScale)
    }

    makespanLine.orig = ((p.MakeSpan.value*xScale).ceil, 0)
    makespanLine.dest = ((p.MakeSpan.value*xScale).ceil, (LineCount+1)*yScale)

    text.text = p.MakeSpan.value.toString
    text.move((p.MakeSpan.value*xScale).ceil.toInt, (LineCount+2)*yScale);
    repaint()
  }
}
