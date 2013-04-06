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

package oscar.cbls.routing.visual

import java.awt.Color
import util.Random

/**
 * This class allows the generation of random colors separate.
 * Following algorithms given by w3.org (http://www.w3.org/TR/AERT#color-contrast)
 * (The range for color difference is 500.)
 */
class ColorManagement {

  val seed = new Random().nextInt()
  var colors:Array[Color] = Array.empty

  def apply(i:Int) = colors(i)

  /*
    Returns an integer giving the proximity between two given colors.
   */
  def colorDifference(c1:Color,c2:Color):Int = {
    (math.max(c1.getRed, c2.getRed) - math.min(c1.getRed, c2.getRed)) +
      (math.max(c1.getGreen, c2.getGreen) - math.min(c1.getGreen, c2.getGreen)) +
      (math.max(c1.getBlue, c2.getBlue) - math.min(c1.getBlue, c2.getBlue))
  }
  /*
    Tells if two given colors are enough different.
   */
  def diffColor(c1:Color,c2:Color):Boolean = {
    if(colorDifference(c1,c2)<= 150) false else true
  }

  /*
    Set n colors enough different.
   */
  def setDifferentColors(n:Int)= {
    var enoughColor:Boolean =  false
    if(n<=21)
      enoughColor =true


    def diff(c1:Color,i:Int):Boolean = {
      var j =0
      while(j<i){
        val c2 = colors(j)
        if(!diffColor(c1,c2))
          return false
        j+=1
      }
      true
    }

    val gen = new Random(seed)
    colors = new Array[Color](n+1)
    colors(0) = Color.white
    var i = 1
    while(i<=n){
      var c:Color = null
      do{
        c = new Color(math.abs(gen.nextInt(1000)) % 256,
          math.abs(gen.nextInt(1000)) % 256,
          math.abs(gen.nextInt(1000)) % 256)
      }
      while(enoughColor && !diff(c,i))
      colors(i) = c
      i += 1
    }
  }


}