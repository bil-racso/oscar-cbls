package oscar.cp.mem

import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.io.Source

/** Hypervolume
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
object Hypervolume {

  case class Point(obj1: Int, obj2: Int)

  def parse(filepath: String): Array[Point] = {
    val lines = Source.fromFile(filepath).getLines.toArray
    val data = lines.map(_.split(" ").map(_.toInt))
    Array.tabulate(data.size)(i => Point(data(i)(0), data(i)(1)))
  }
  
  def simple2DHypervolume(ps: Array[Point], ref: Point): Double = {  
    var vol = 0
    for (i <- 0 until ps.size) {
      val x = ps(i).obj1 - (if (i == 0) ref.obj1 else ps(i - 1).obj2)
      val y = ps(i).obj2 - ref.obj1
      vol += x * y
    }
    vol
  }
  
  def main(args: Array[String]) {   
    //val data = parse(args(0))
    //val ref = Point(args(1).toInt, args(2).toInt)   
    val data = parse("/Users/renaudhartert/Desktop/Kro/ListNDS_2PPLS_Kroab100_2.txt")
    val ref = Point(176436, 178446)
    println(simple2DHypervolume(data, ref))//*100000000)
  }
}