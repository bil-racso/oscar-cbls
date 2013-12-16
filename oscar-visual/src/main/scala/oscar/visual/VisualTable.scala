package oscar.visual

import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.table.AbstractTableModel
import javax.swing.JTable
import java.awt.Component
import javax.swing.table.TableCellRenderer
import javax.swing.JComponent
import java.awt.Color
import javax.swing.TransferHandler
import java.awt.datatransfer.Clipboard
import java.awt.Toolkit
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.JScrollPane

class VisualTable(n: Int, m: Int) extends JPanel(new BorderLayout()) {

  private val data: Array[Array[Object]] = Array.fill(n+1, m+1)(null)
  private val colNames: Array[Object] = Array.fill(m+1)(null)
  private val bgCol: Array[Array[Color]] = Array.fill(n+1, m+1)(null)
  private val fgCol: Array[Array[Color]] = Array.fill(n+1, m+1)(null)
  
  private val model = new AbstractTableModel() {   
    override def getColumnCount: Int = m + 1  
    override def getRowCount: Int = n + 1  
    override def getValueAt(row: Int, column: Int) = data(row)(column)    
    override def getColumnClass(column: Int) = classOf[Object]
    override def isCellEditable(row: Int, column: Int): Boolean = false   
    override def setValueAt(value: Object, row: Int, column: Int): Unit = {
      data(row)(column) = value
    }
  }
  
  val jtable = new JTable(model) {   
    override def prepareRenderer(renderer: TableCellRenderer, row: Int, column: Int): Component = {
      val c = super.prepareRenderer(renderer, row, column)
      if (c.isInstanceOf[JComponent]) {
        val value = getValueAt(row, column)
        if (value != null) {
          val jc = c.asInstanceOf[JComponent]
          jc.setToolTipText(getValueAt(row, column).toString)
        }
      }
      c
    }
  }
  
  val th: TransferHandler = getTransferHandler()
  if (th != null) {
    val cb: Clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()
    th.exportToClipboard(this, cb, TransferHandler.COPY)
  }
  
  jtable.setDefaultRenderer(classOf[Object], new DefaultTableCellRenderer() {
    override def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) = {
      val c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      if (bgCol(row)(column) != null) {
        c.setBackground(bgCol(row)(column))
      }
      else {
        c.setBackground(Color.white)
      }
      if (fgCol(row)(column) != null) {
        c.setForeground(fgCol(row)(column))
      }
      else {
        c.setForeground(Color.black)
      }
      c
    }  
  })
  
  setValueAt("", -1, -1)
  setColorAt(Color.white, -1, -1)
  setFGColorAt(Color.black, -1, -1)
  
  for (i <- 0 until n) {
    setValueAt(i.toString, i, -1)
    setColorAt(Color.BLACK, i, -1)
    setFGColorAt(Color.WHITE, i, -1)
  }
  
  for (j <- 0 until m) {
    setValueAt(j.toString, -1, j)
    setColorAt(Color.BLACK, -1, j)
    setFGColorAt(Color.WHITE, -1, j)
  }
  
  jtable.getTableHeader().setVisible(false)
  
  val jscr = new JScrollPane(jtable,javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
  
  jtable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
  
  add(jscr, BorderLayout.CENTER)
  
  def getTable: JTable = jtable
  
  def getModel: AbstractTableModel = model
  
  def setValueAt(value: Object, row: Int, column: Int): Unit = {
    data(row+1)(column+1) = value
    model.fireTableDataChanged()
  }
  
  def setColorAt(color: Color, row: Int, column: Int): Unit = {
    bgCol(row+1)(column+1) = color
    model.fireTableDataChanged()
  }
  
  def setFGColorAt(color: Color, row: Int, column: Int): Unit = {
    fgCol(row+1)(column+1) = color
    model.fireTableDataChanged()
  }
  
  def setColumnName(value: Object, column: Int): Unit = {
    setValueAt(value, -1, column)
  }
  
  def setRowName(value: Object, row: Int): Unit = {
    setValueAt(value, row, -1)
  }
}

object VisualTableExample extends App {
  
  val frame = VisualFrame("Example")
  val inFrame = frame.createFrame("Table")
  val table = new VisualTable(3, 4)
  table.setValueAt("toto", 0, 0)
  table.setValueAt("1", 1, 0)
  inFrame.add(table)
  inFrame.pack
}


















