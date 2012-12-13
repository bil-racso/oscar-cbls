/*******************************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  ******************************************************************************/

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing
import io._
import java.io._
import util.Random
import oscar.cbls.routing.model._

/**
 * Constructor of a point of plane given by his coordinates.
 * @param name : label of point.
 * @param long : longitude of point (or more generally vertical axis).
 * @param lat : latitude of point (or more generally horizontal axis).
 */
class Point(val name:String,val long:Double,val lat:Double)

/**
 * Import towns of Belgium from a db.
 */
class BelgiumInstance {
  var towns:List[Point] = List.empty
  val path = new File("").getAbsolutePath
  var file = Source.fromFile(path+"\\src\\main\\scala\\oscar\\cbls\\routing\\villesbelgique")
  for(line <- file.getLines()){
    val words = line.split(" ")
    if(words.length == 8)
      towns = new Point(words(1).toString,words(6).toDouble,words(7).toDouble) :: towns
  }
}

/**
 * Creates instances of VRP from Belgian cities.
 */
object BelgiumInstance{
  val towns:Array[Point] = new BelgiumInstance().towns.toArray

  /**
   * Creates a random instance of n points.
   * @param n : number of points of the instance.
   * @return array of n random belgian towns.
   */
  def random(n:Int):Array[Point]={
    val random = Random.shuffle(Range(0,towns.length))
    val randomTowns:Array[Point] = new Array[Point](n)
    for(i <- Range(0,n)) randomTowns(i) = towns(random(i))
    randomTowns
  }

  /**
   * Creates a fixed instance of n points.
   * @param n : number of points of the instance.
   * @return array of n belgian towns.
   */
  def instance_1(n:Int):Array[Point] = {
    assert(n<towns.length/2)
    val seed = 0
    val instanceTowns = new Array[Point](n)
    val range = (seed until seed+n).map(x => 2*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  /**
   * Creates a fixed instance of n points.
   * @param n : number of points of the instance.
   * @return array of n belgian towns.
   */
  def instance_2(n:Int):Array[Point] = {
    assert(n<(towns.length-1000)/2)
    val seed = 1000
    val instanceTowns = new Array[Point](n)
    val range = (seed until seed +n).map(x => 2*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  /**
   * Creates a fixed instance of n points.
   * @param n : number of points of the instance.
   * @return array of n belgian towns.
   */
  def instance_3(n:Int):Array[Point] = {
    assert(n<(towns.length-500)/3)
    val seed = 500
    val instanceTowns = new Array[Point](n)
    val range = (seed until seed+n).map(x => 3*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  /**
   * Creates a fixed instance of n points.
   * @param n : number of points of the instance.
   * @return array of n belgian towns.
   */
  def instance_4(n:Int):Array[Point] = {
    assert(n<(towns.length-500)/4)
    val seed = 500
    val instanceTowns = new Array[Point](n)
    val range = (seed until seed+n).map(x => 4*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  /**
   * Creates a fixed instance of n points.
   * @param n : number of points of the instance.
   * @return array of n belgian towns.
   */
  def instance_5(n:Int):Array[Point] = {
    assert(towns.length> (n*n))
    val seed = 0
    val instanceTowns = new Array[Point](n)
    val range = (seed until seed+n).map(x => x*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
}

/**
 * Generates instances of VRP on a given range.
 */
object InstanceVisualVRP{

  /**
   * Generates random instances of a given VRP on a given range.
   * @param vrp : the given VRP.
   * @param n : 0 for differents deposits, other integer for same deposits.
   * @param xMax : the integer that specifies the interval of the abscissa.
   * @param yMax : the integer that specifies the interval of the ordinate.
   * @param seed : the seed of the generator.
   * @return array of random points.
   */
  def getInstance(vrp:VRP,n:Int,xMax:Int,yMax:Int,seed:Int = 0):Array[Point]=
    n match {
      case 0 => random(vrp.N,xMax,yMax,seed)
      case n => randomSameDepot(vrp.N,vrp.V,xMax,yMax,seed)
    }

  /**
   * Generates random instances of a given VRP on a given range.
   * @param vrp : the given VRP.
   * @param n : 0, 1 and 2 for differents deposits, other integer for same deposits.
   * @param xMax : the integer that specifies the interval of the abscissa.
   * @param yMax : the integer that specifies the interval of the ordinate.
   * @param seed : the seed of the generator.
   * @return array of random points.
   */
  def getInstanceEasy(vrp:VRP,n:Int,xMax:Int,yMax:Int,seed:Int = 0):Array[Point]=
    n match {
      case 0 => random(vrp.N,xMax,yMax,1)
      case 1 =>  random(vrp.N,xMax,yMax,1)
      case 2 =>  random(vrp.N,xMax,yMax,2)
      case n => randomSameDepot(vrp.N,vrp.V,xMax,yMax,n+1)
  }

  /**
   * Generates random instances of n points and v vehicles on a given range.
   * Theses instances have differents deposits.
   * @param n : the number of points of the instance.
   * @param xMax : the integer that specifies the interval of the abscissa.
   * @param yMax : the integer that specifies the interval of the ordinate.
   * @param seed : the seed of the generator.
   * @return array of random points.
   */
  def random(n:Int,xMax:Int,yMax:Int,seed:Int = 0):Array[Point]={
    var gen = new Random()
    if (seed != 0) {gen = new Random(seed)}
    val randomTowns = new Array[Point](n)
    for (i <- 0 until n){
      randomTowns(i) = new Point(""+i,gen.nextInt(xMax)+50,gen.nextInt(yMax)+50)
    }
    randomTowns
  }

  /**
   * Generates random instances of n points and v vehicles on a given range.
   *  Theses instances have a single deposit.
   * @param n : the number of points of the instance.
   * @param v : the number of vehicles of the instance.
   * @param xMax : the integer that specifies the interval of the abscissa.
   * @param yMax : the integer that specifies the interval of the ordinate.
   * @param seed : the seed of the generator.
   * @return array of random points.
   */
  def randomSameDepot(n:Int,v:Int,xMax:Int,yMax:Int,seed:Int = 0):Array[Point] = {
    var gen = new Random()
    if (seed != 0) {gen = new Random(seed)}
    val randomTowns = new Array[Point](n)
    val depot = new Point("Dépot",gen.nextInt(xMax)+50,gen.nextInt(yMax)+50)
    for (i <- 0 until v) randomTowns(i) = depot
    for (i <- v until n){
      randomTowns(i) = new Point(""+i,gen.nextInt(xMax)+50,gen.nextInt(yMax)+50)
    }
    randomTowns
  }

}


