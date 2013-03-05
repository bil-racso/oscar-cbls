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


import java.awt.Color
import java.awt.BorderLayout
import java.awt.Component
import java.awt.FlowLayout
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Image
import java.awt.Toolkit
import java.awt.datatransfer.Clipboard
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.MouseEvent
import java.awt.event.MouseMotionListener
import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.util.LinkedList
import javax.swing.ImageIcon
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JInternalFrame
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTable
import javax.swing.TransferHandler
import javax.swing.event.CellEditorListener
import javax.swing.table.AbstractTableModel
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.TableCellRenderer
import java.awt.Shape
import java.awt.geom.AffineTransform
import javax.swing.JSlider
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent

class VisualDrawing(flipped:Boolean) extends JPanel (new BorderLayout()) {



  
	var drawingPanel:JPanel = new JPanel() {
			override def paintComponent(g:Graphics) {
			    val maxX = shapes.map(s => s.shape.getBounds().x).max
			    val maxY = shapes.map(s => s.shape.getBounds().y).max
			    val scx = getWidth()/(maxX.toDouble*1.1)
			    val scy = getHeight()/(maxY.toDouble*1.1)
			    
				if (flipped) {
					g.translate(0,getHeight()); 
					(g.asInstanceOf[Graphics2D]).scale(scx, -scy);
				} else {
				  (g.asInstanceOf[Graphics2D]).scale(scx, scy);
				}
				super.paintComponent(g);
				for (s <- shapes) {
					s.draw(g.asInstanceOf[Graphics2D]);
				}
			}
		}


	var shapes:Array[ColoredShape[Shape]] = Array();
		
	drawingPanel.addMouseMotionListener(new MouseMotionListener() {
		override def mouseMoved(e:MouseEvent) {
			drawingPanel.setToolTipText("");
			for (s <- shapes) {
				s.showToolTip(e.getPoint());
			}
		}
		
		override def mouseDragged(arg0:MouseEvent) {
		}
	})
	
	
	drawingPanel.setBackground(Color.white)

	add(drawingPanel, BorderLayout.CENTER)


	val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
	
	buttonPanel.setBackground(Color.white)
	
	
	def showToolTip(text:String) {
		drawingPanel.setToolTipText(text);
	}


	def addShape(s:ColoredShape[Shape]) {
		shapes :+= s
		repaint();
	}

}

object VisualDrawingTest{
  def main(args : Array[String]) {
		
		val f = new VisualFrame("toto");
		val d = new VisualDrawing(false);
		val inf = f.createFrame("Drawing");
		inf.add(d);
		f.pack();
		val r = new Rectangle2D.Double(0, 0,100,100);
		
		val rect = new ColoredShape[Rectangle2D](d,r);
		
	    
		val l = new ColoredShape[Line2D](d,new Line2D.Double(0, 0, 100, 100));
		
		
		try {
			Thread.sleep(1000);
		} catch{
			case e : InterruptedException => e.printStackTrace();
		}
		rect.innerCol=Color.red;
		
		
		
	}
}
