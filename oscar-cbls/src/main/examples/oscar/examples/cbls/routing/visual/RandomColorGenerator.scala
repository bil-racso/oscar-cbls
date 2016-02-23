package oscar.examples.cbls.routing.visual

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

import java.awt.Color

/**
  * @author fabian.germeau@student.vinci.be
  */
object RandomColorGenerator {
  def generateRandomColors(number:Int): Array[Color] ={
    val maxColorNumber = getMaxColorNumber(number)
    val colorValues = new Array[Color](Math.pow(maxColorNumber,3).toInt)
    for(c1 <- 0 until maxColorNumber){
      val r = 255 / maxColorNumber*c1
      for(c2 <- 0 until maxColorNumber){
        val g = 255 / maxColorNumber*c2
        for(c3 <- 0 until maxColorNumber){
          val b = 255 / maxColorNumber*c3
          val i = c1*maxColorNumber*maxColorNumber + c2*maxColorNumber + c3
          colorValues(i) = new Color(r,g,b)
        }
      }
    }
    colorValues
  }

  def getMaxColorNumber(number:Int,exp:Int = 1):Int = {
    if(Math.pow(exp,3) < number)
      getMaxColorNumber(number,exp+1)
    else
      exp
  }
}
