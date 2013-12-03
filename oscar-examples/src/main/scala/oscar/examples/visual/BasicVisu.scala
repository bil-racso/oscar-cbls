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
package oscar.examples.visual

import oscar.visual.VisualFrame
import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualRectangle

object BasicVisu {

  def main(args: Array[String]): Unit = {

    val f = VisualFrame("Basic");
    val tb = f.createToolBar()
    tb.addButton("foo", println("bar"))
    val d = VisualDrawing(false);
    val inf = f.createFrame("Rectangle");
    inf.add(d);
    val rect = new VisualRectangle(d, 50, 50, 100, 50);
    rect.toolTip = "Hello";

  }

}
