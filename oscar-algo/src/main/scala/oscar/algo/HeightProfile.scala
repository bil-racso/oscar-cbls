package oscar.algo

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
object HeightProfile extends App {
  
  /**
   * @param rectangles = an array of (start,duration,height)
   * @return an array of points (start,duration,height) constituting the cumulated profile
   */
  def computeProfile(rectangles: Array[(Int,Int,Int)]): Array[(Int,Int,Int)] = {
    class Rect(val start: Int,val dur: Int,val height: Int) {
      def end = start + dur
    }
    val rects = rectangles.map(r => new Rect(r._1,r._2,r._3))
    val heap = PriorityQueue[(Int,Int)]()
    val n = rects.size
    for (i <- 0 until n) {
      val r = rects(i)
      heap.enqueue((-r.start,r.height))
      heap.enqueue((-r.end,-r.height))
    }
    
    def topTime = - heap.head._1
    
    var t = topTime
    var h = 0
    var res = new ArrayBuffer[(Int,Int,Int)]()
    
    while (heap.nonEmpty) {
      while (heap.nonEmpty && topTime == t) {
        val (_,hd) = heap.dequeue()
        h += hd
      }
      if (heap.nonEmpty) {
        val t1 = topTime
        res.append((t,t1-t,h))
        t = t1
      } else {
        assert(h == 0)
      }
    }
    res.toArray
  }
  
  
  val a = Array((5,10,6),(11,10,6))
  // (5,6,6),(11,4,12),(15,6,6)
  println(computeProfile(a).mkString(","))

}