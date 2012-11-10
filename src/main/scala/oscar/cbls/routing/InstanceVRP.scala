package oscar.cbls.routing

import io._
import java.io._
import util.Random
import math._

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

object InstanceVisualVRP{

  def random(n:Int,xMax:Int,yMax:Int,seed:Int = 0):Array[Town]={
    var gen = new Random()
    if (seed != 0) {gen = new Random(seed)}
    val randomTowns = new Array[Town](n)
    for (i <- 0 until n){
      randomTowns(i) = new Town(""+i,gen.nextInt(xMax)+50,gen.nextInt(yMax)+50)
    }
    randomTowns
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
    val range = (seed until seed+n).map(x => 2*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  def instance_2(n:Int):Array[Town] = {
    assert(n<(towns.length-1000)/2)
    val seed = 1000
    val instanceTowns = new Array[Town](n)
    val range = (seed until seed +n).map(x => 2*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
   }
  def instance_3(n:Int):Array[Town] = {
    assert(n<(towns.length-500)/3)
    val seed = 500
    val instanceTowns = new Array[Town](n)
    val range = (seed until seed+n).map(x => 3*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  def instance_4(n:Int):Array[Town] = {
    assert(n<(towns.length-500)/4)
    val seed = 500
    val instanceTowns = new Array[Town](n)
    val range = (seed until seed+n).map(x => 4*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }
  def instance_5(n:Int):Array[Town] = {
    assert(towns.length> (n*n))
    val seed = 0
    val instanceTowns = new Array[Town](n)
    val range = (seed until seed+n).map(x => x*x)
    for (i <- 0 until n) instanceTowns(i) = towns(range(i))
    instanceTowns
  }

}


