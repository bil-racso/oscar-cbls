package oscar.cbls.routing.visual

import javax.swing.{JLabel, JFrame, JApplet}
import java.awt.Dimension


/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 8/11/12
 * Time: 9:11
 * To change this template use File | Settings | File Templates.
 */
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


class AppletVRP extends JApplet {

  override def init() {

    setSize(new Dimension(1000,600))
    setContentPane(PanelVRP.PanelVRP)
    setName("VRP Routing")
    setVisible(false)
  }
}

