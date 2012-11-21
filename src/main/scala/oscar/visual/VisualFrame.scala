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
package oscar.visual;

import java.awt._
import java.awt.event._
import javax.swing._
import scala.swing.BoxPanel
import scala.swing.Orientation


class VisualFrame(title:String, nbLines:Int, nbCols:Int) extends JFrame(title) {

	val desktop = new JDesktopPane()
	
	
	desktop.setBackground(Color.white)
	
	val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
	var n = 0
	var w = screenSize.width/nbCols
	var h = screenSize.height/nbLines	

	
	val content = getContentPane()
	content.setBackground(Color.white)
	content.add(new JScrollPane(desktop))
	setSize(screenSize)
	setMinimumSize(new java.awt.Dimension(400,300))
	setVisible(true)
	
	addWindowListener(new WindowAdapter() {
		 override def windowClosing(event:WindowEvent) {
			  System.exit(0);
		  }
	})
	
	
	def this(title:String) = this(title,2,2)
	
	
	
	def createMenuBar() : JMenuBar = {
		val menuBar = new JMenuBar()
		val menu = new JMenu("Frame")
		menu.setMnemonic(KeyEvent.VK_N)
		val menuItem = new JMenuItem("New IFrame")
		menuItem.setMnemonic(KeyEvent.VK_N)
		menuItem.addActionListener(new ActionListener() {

			override def actionPerformed(e:ActionEvent) {
				createFrame("sub frame")
			}
		})
		menu.add(menuItem)
		menuBar.add(menu)
		menuBar
	}
	
	def  createFrame( title:String):JInternalFrame = {
		val c = n%nbCols
		val l = n/nbCols
		
		val frame = new JInternalFrame(title, true, false, true, true)
		
		frame.setLocation(c*w,l*h)
		frame.setPreferredSize(new java.awt.Dimension(w,h))
		frame.setMinimumSize(new java.awt.Dimension(w,h))
		frame.setSize(w,h)
		frame.setBackground(Color.white)
		frame.setVisible(true)
		desktop.add(frame)
		frame.moveToFront()
		n+=1
		frame
	}	

	def  createToolBar():VisualToolBar = {
		val toolbar = new VisualToolBar()
		content.add(toolbar,BorderLayout.NORTH)
		toolbar
	}
	  
	  


}
object VisualFrame{
  	def main(args : Array[String]) {
		val frame = new VisualFrame("My Frame")
		val subframe = frame.createFrame("My Sub-frame")
	}
}