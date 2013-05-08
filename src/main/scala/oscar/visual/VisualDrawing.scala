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

class VisualDrawing(flipped: Boolean, resizable: Boolean = true) extends JPanel(new BorderLayout()) {

  require(!flipped || resizable)

  setBackground(Color.white)

  def scx = getWidth() / (maxX.toDouble * 1.1)
  def scy = getHeight() / (maxY.toDouble * 1.1)

  var shapes: Array[ColoredShape[Shape]] = Array();

  def update{
    /*
     * smouthuy: if anyone knows where it would be better to set the preferred size, let's change this
     * . Setting this in the repaint method is too costly
     */
	  if ( shapes != null && shapes.filter(_.shape != null).nonEmpty) this.setPreferredSize(new java.awt.Dimension(maxX,maxY))
  }  
  
  def maxX = shapes.filter(_.shape != null).map { s =>
          val b = s.shape.getBounds()
          b.x + b.width
        }.max
        def maxY = shapes.filter(_.shape != null).map { s =>
          val b = s.shape.getBounds()
          b.y + b.height
        }.max
  
    var drawingPanel: JPanel = new JPanel() {
    override def paintComponent(g: Graphics) {
      
      val s = shapes.filter(_.shape != null)

      if (!s.isEmpty) {
        
        if (flipped) {
          g.translate(0, getHeight());
          (g.asInstanceOf[Graphics2D]).scale(scx, -scy);
        } else {
          if (resizable) {
            (g.asInstanceOf[Graphics2D]).scale(scx, scy);
          } else{
        	  (g.asInstanceOf[Graphics2D]).scale(1.0,1.0);
            
          }
        }
        super.paintComponent(g);
        for (s <- shapes) {
          s.draw(g.asInstanceOf[Graphics2D]);
        }
      }

    }
  }


  drawingPanel.addMouseMotionListener(new MouseMotionListener() {
    override def mouseMoved(e: MouseEvent) {
      drawingPanel.setToolTipText("");
      for (s <- shapes) {
        s.showToolTip(e.getPoint());
      }
    }

    override def mouseDragged(arg0: MouseEvent) {
    }
  })

  drawingPanel.setBackground(Color.white)

  add(drawingPanel, BorderLayout.CENTER)

  val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))

  buttonPanel.setBackground(Color.white)

  def showToolTip(text: String) {
    drawingPanel.setToolTipText(text);
  }

  def addShape(s: ColoredShape[Shape]) {
    shapes :+= s
    repaint()
  }

  def clear() {
    shapes = Array()
    revalidate()
    repaint()
  }

}

object VisualDrawingTest {
  def main(args: Array[String]) {

    val f = new VisualFrame("toto");
    val d = new VisualDrawing(false);
    val inf = f.createFrame("Drawing");
    inf.add(d);
    f.pack();
    val r = new Rectangle2D.Double(0, 0, 100, 100);

    val rect = new ColoredShape[Rectangle2D](d, r);

    val l = new ColoredShape[Line2D](d, new Line2D.Double(0, 0, 100, 100));

    try {
      Thread.sleep(1000);
    } catch {
      case e: InterruptedException => e.printStackTrace();
    }
    rect.innerCol = Color.red;

  }
}
