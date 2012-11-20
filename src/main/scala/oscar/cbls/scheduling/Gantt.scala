package oscar.cbls.scheduling

import java.awt.Color
import oscar.visual.{VisualLine, VisualText, VisualRectangle, VisualDrawing, VisualUtil}
import java.awt.Component._
import oscar.cbls.scheduling._

class Gantt(p:Planning) extends VisualDrawing(false) {

  var LineCount = 0
  val LineArray:Array[Int] = makeLineArray(p)
  val colors:Array[Color] = Array.tabulate(p.TaskArray.size)(_ => VisualUtil.getRandomColor())
  def makeLineArray(p:Planning):Array[Int] = {
    var currentline = -1
    var Front:List[Int] = List.empty
    val GantLines:Array[Int] = Array.tabulate(p.TaskArray.size)((i:Int)
    => (if (p.TaskArray(i).StaticPredecessors.isEmpty){
      currentline +=1
      Front = i :: Front
      currentline
    }else{
      -1
     }))
    LineCount = currentline -1
    def decorate(startid:Int){
      val task:Task = p.TaskArray(startid)
      for (t <- task.AllSucceedingTasks.value){
        val succeedingTask:Task = p.TaskArray(t)
        if (GantLines(succeedingTask.TaskID) == -1){
          GantLines(succeedingTask.TaskID) = GantLines(task.TaskID)
          decorate(t)
        }
      }
    }

    for (startid <- Front) decorate(startid)
    GantLines
  }
  
  private val rectangles : Array[VisualRectangle] = Array.tabulate(p.TaskArray.size)(a => {
    val rect = new VisualRectangle(this, 0, 0, 0, 0)
    rect.innerCol = colors(a)
    rect.toolTip = p.TaskArray(a).name
    rect
  })

  private val text : VisualText = new VisualText(this, 50, 50, "")
  text.innerCol = Color.RED
  text.setCentered(true)

  private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
  makespanLine.outerCol = Color.RED;

  def update(xScale : Float, yScale: Int) {

    for (task <- p.Tasks) {

      val i = task.TaskID
      rectangles(i).width = ((task.duration.value)*xScale).ceil
      rectangles(i).height = yScale
      rectangles(i).innerCol = colors(i)
      rectangles(i).move((task.EarliestStartDate.value*xScale).ceil, LineArray(task.TaskID)*yScale)
    }

    makespanLine.orig = ((p.MakeSpan.value*xScale).ceil, 0)
    makespanLine.dest = ((p.MakeSpan.value*xScale).ceil, (LineCount+1)*yScale)

    text.setText(p.MakeSpan.value.toString)
    text.move((p.MakeSpan.value*xScale).ceil.toInt, (LineCount+2)*yScale);
    repaint()
  }
}
