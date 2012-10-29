package oscar.cbls.routing

import io._
import java.io._
import util.Random

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 23/10/12
 * Time: 21:55
 * To change this template use InstanceVRP | Settings | InstanceVRP Templates.
 */
class Town(val name:String,val long:Double,val lat:Double)

class InstanceVRP {
  var towns:List[Town] = List.empty
  val path = new File("").getAbsolutePath
  var file = Source.fromFile(path+"\\src\\main\\scala\\oscar\\cbls\\routing\\villesbelgique")
  for(line <- file.getLines()){
    val words = line.split(" ")
    if(words.length == 8)
      towns = new Town(words(1).toString,words(6).toDouble,words(7).toDouble) :: towns
  }
}

object InstanceVRP{
  val towns:Array[Town] = new InstanceVRP().towns.toArray

  def random(n:Int):Array[Town]={
    val random = Random.shuffle(Range(0,towns.length))
    val randomTowns:Array[Town] = new Array[Town](n)
    for(i <- Range(0,n)) randomTowns(i) = towns(random(i))
    randomTowns
  }

  def instance_1(n:Int):Array[Town] = {
    assert(n<towns.length/2)
    val seed = 0
    val instanceTowns = new Array[Town](n)
    val range = (seed until n).map(x => 2*x)
    for (i <- seed until seed+n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  def instance_2(n:Int):Array[Town] = {
    assert(n<(towns.length-1000)/2)
    val seed = 1000
    val instanceTowns = new Array[Town](n)
    val range = (seed until n).map(x => 2*x)
    for (i <- seed until seed+n) instanceTowns(i) = towns(range(i))
    instanceTowns
   }
  def instance_3(n:Int):Array[Town] = {
    assert(n<(towns.length-500)/3)
    val seed = 500
    val instanceTowns = new Array[Town](n)
    val range = (seed until n).map(x => 3*x)
    for (i <- seed until seed+n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  def instance_4(n:Int):Array[Town] = {
    assert(n<(towns.length-500)/4)
    val seed = 500
    val instanceTowns = new Array[Town](n)
    val range = (seed until n).map(x => 4*x)
    for (i <- seed until seed+n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  def instance_5(n:Int):Array[Town] = {
    assert(n<towns.length/(n*n))
    val seed = 0
    val instanceTowns = new Array[Town](n)
    val range = (seed until n).map(x => x*x)
    for (i <- seed until seed+n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }

}


