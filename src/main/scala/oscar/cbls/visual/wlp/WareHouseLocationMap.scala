package oscar.cbls.visual.wlp

import java.awt.geom.Rectangle2D
import java.awt.{Dimension, BorderLayout, Color}
import java.awt.geom.Line2D.Double
import javax.swing.JFrame
import oscar.cbls._
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualRectangle, VisualCircle, VisualLine, VisualShape}

import scala.collection.immutable.SortedSet

class WareHouseLocationMap(deliveryCoordinates:Array[(Long,Long)],
                           wareHouseCoordinates:Array[(Long,Long)],
                           distanceCostD2W:Array[Array[Long]],
                           warehouseCosts:Array[Long])
  extends VisualDrawing(false,false){

  val maxX = math.max(deliveryCoordinates.map(_._1).max,wareHouseCoordinates.map(_._1).max)
  val maxY = math.max(deliveryCoordinates.map(_._2).max,wareHouseCoordinates.map(_._2).max)

  val w = wareHouseCoordinates.length
  val d = deliveryCoordinates.length

  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }

  var prevOpenWarehouse:SortedSet[Long] = SortedSet.empty
  var prevNearestOpenWarehouse = Array.fill[Long](d)(-1)

  def redraw(openWarehouses:SortedSet[Long],boldChanges:Boolean=true,hideClosedWarehouses:Boolean = false){
    val closestWarehouses:Array[Long] = Array.tabulate(d)(nearestOpenWareHouse(openWarehouses,_))
    drawMap(closestWarehouses,openWarehouses,prevOpenWarehouse,prevNearestOpenWarehouse,boldChanges,hideClosedWarehouses)
    prevOpenWarehouse = openWarehouses
    prevNearestOpenWarehouse = closestWarehouses
  }

  /**
   * @param d
   * @return -1L is no open warehouse
   */
  private def nearestOpenWareHouse(openWarehouses:SortedSet[Long],d:Long):Long = {
    var closestW = -1
    var minDistance = Long.MaxValue
    for(w <- openWarehouses){
      val distance = distanceCostD2W(d)(w)
      if(distance < minDistance){
        closestW = w
        minDistance = distance
      }
    }
    closestW
  }

  private def drawMap(closestWarehouses:Array[Long],
                      openWarehouses:SortedSet[Long],
                      prevOpenWarehouse:SortedSet[Long],
                      prevClosestWarehouse:Array[Long],
                      boldChanges:Boolean,
                      hideClosedWarehouses:Boolean) ={

    val xMultiplier = this.getWidth.toDouble / maxX.toDouble
    val yMultiplier = this.getHeight.toDouble / maxY.toDouble

    super.clear(false)

    //drawind links
    if (openWarehouses.nonEmpty) {
      for (delivery <- 0 until d) {
        val warehouse = closestWarehouses(delivery)
        if (warehouse != -1) {
          val line = new VisualLine(this, new Double(
            deliveryCoordinates(delivery)._1 * xMultiplier,
            deliveryCoordinates(delivery)._2 * yMultiplier,
            wareHouseCoordinates(warehouse)._1 * xMultiplier,
            wareHouseCoordinates(warehouse)._2 * yMultiplier))
          line.dashed = true
        }
      }
    }

    def drawWarehouse(warehouse:Int,color:Color,focus:Boolean){
      val tempPoint = new VisualRectangle(this, new Rectangle2D.Double(
        wareHouseCoordinates(warehouse)._1 * xMultiplier - 4,
        wareHouseCoordinates(warehouse)._2 * yMultiplier - 4,
        8,
        8))
      tempPoint.innerCol_$eq(color)
      if(focus){
        tempPoint.borderWidth = 4
      }
      tempPoint.toolTip = "warehouseCost:" + warehouseCosts(warehouse)
    }

    //drawing warehouses
    if(!hideClosedWarehouses) {
      for (warehouse <- 0 until w if !(openWarehouses contains warehouse)) {
        drawWarehouse(warehouse, Color.PINK, boldChanges && (prevOpenWarehouse contains warehouse))
      }
    }

    for(warehouse <- 0 until w if openWarehouses contains warehouse) {
      drawWarehouse(warehouse, Color.green, boldChanges && !(prevOpenWarehouse contains warehouse))
    }

    //drawing delivery points
    for(delivery <- 0 until d){
      val warehouse = closestWarehouses(delivery)
      val changed = warehouse != prevClosestWarehouse(delivery)
      val color =
        if(warehouse == -1)Color.red
        else Color.black
      val tempPoint = new VisualCircle(this,
        deliveryCoordinates(delivery)._1 * xMultiplier,
        deliveryCoordinates(delivery)._2 * yMultiplier,2)
      tempPoint.innerCol_$eq(color)
      if(boldChanges && changed) tempPoint.setRadius(4)
      if(warehouse != -1){
        tempPoint.toolTip = "distanceCost:" + distanceCostD2W(delivery)(warehouse)
      }
    }

    repaint()
  }
}
