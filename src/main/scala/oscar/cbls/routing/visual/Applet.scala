/*******************************************************************************
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
 ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/
package oscar.cbls.routing.visual
import javax.swing.JApplet
import java.awt.Dimension


/** this is an applet demonstrating the routing engine.
  * it takes a parameter: light which can be true or false
  * if true, it provides a lightweight demonstrator, if false, it provides a richer demonstrator
  */
class Applet extends JApplet with App {

  override def init(){
    val easyMode = ("true" == this.getParameter("light"))
    val myPanelVRP = new PanelVRP(easyMode)
    setContentPane(myPanelVRP)
    setSize(new Dimension(800,500))

    setName("VRP Routing")
    setVisible(false)
  }
}

