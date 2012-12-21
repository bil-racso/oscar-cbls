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
import oscar.reversible.ReversibleSearchNode
import ie.ucc.cccc.viz.Viz
import org.apache.batik.swing.JSVGCanvas


/**
 * Class to show a visual search tree (currently only available for binary search trees)
 * @author Pierre Schaus pschaus@gmail.com
 */


class VisualSearchTree(node: ReversibleSearchNode) extends JPanel (new BorderLayout()) {
	node.tree.save("tree.xml")
	Viz.runViz("configuration.xml", "tree.xml", "res.viz")
    val c = new JSVGCanvas()
    c.setURI(new File("tree.svg").toURL().toString)
	add(c)
}

object VisualSearchTree{
  def main(args : Array[String]) {
		
		
		
		
		
  }
}
