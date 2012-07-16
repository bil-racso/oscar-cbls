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


import java.awt.Color;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import javax.swing.TransferHandler;
import javax.swing.event.CellEditorListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;



public class VisualTable extends JPanel  {

	private static final long serialVersionUID = 1L;
	int n,m;    
	Object[][] data;
	Object [] colnames;
	JTable jtable;
	AbstractTableModel model;
	Color [][] bgCol, fgCol;

	public VisualTable(final int n,final int m) {
		super(new BorderLayout());

		this.n = n;
		this.m = m;

		data = new Object[n+1][m+1];
		colnames = new Object[m+1];
		bgCol = new Color[n+1][m+1];
		fgCol = new Color[n+1][m+1];

		model = new AbstractTableModel() {

			public int getColumnCount() {
				return m+1;
			}

			public int getRowCount() {
				return n+1;
			}

			public Class getColumnClass( int column ) {
				return Object.class;
			}

			public Object getValueAt(int row, int col) {
				return data[row][col];
			}

			public boolean isCellEditable(int row, int col) {
				return false;
			}

			@Override
			public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
				data[rowIndex][columnIndex] = aValue;
			}

		};
		jtable = new JTable(model) {
			public Component prepareRenderer(TableCellRenderer renderer,
					int rowIndex, int vColIndex) {
				Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
				if (c instanceof JComponent) {
					Object val = getValueAt(rowIndex, vColIndex);
					if (val != null) {
						JComponent jc = (JComponent)c;
						jc.setToolTipText(getValueAt(rowIndex, vColIndex).toString());
					}
				}
				return c;
			}
		};



		//jtable.getColumnModel().getColumn(m-1).setPreferredWidth(800);

		TransferHandler th = getTransferHandler();
		if (th != null) {
			Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
			th.exportToClipboard(this, cb, TransferHandler.COPY);
		}


		jtable.setDefaultRenderer(Object.class, new DefaultTableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
			{
				Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (bgCol[row][column] != null) {
					c.setBackground(bgCol[row][column]);
				}
				else {
					c.setBackground(Color.white);
				}
				if (fgCol[row][column] != null) {
					c.setForeground(fgCol[row][column]);
				}
				else {
					c.setForeground(Color.black);
				}
				return c;
			}

		}); 

		setValueAt("", -1, -1);
		setColorAt(Color.white,-1,  -1);
		setFGColorAt(Color.black,-1,  -1);

		// initialize column and row headers
		for (int i = 0; i < n; i++) { // row
			setValueAt(i, i, -1);
			setColorAt(Color.BLACK,i,  -1);
			setFGColorAt(Color.WHITE, i, -1);
		}
		for (int i = 0; i < m; i++) { // col
			setValueAt(i, -1, i);
			setColorAt(Color.BLACK, -1, i);
			setFGColorAt(Color.WHITE, -1, i);
		}

		jtable.getTableHeader().setVisible(false);
		JScrollPane jscr = new JScrollPane(jtable);
		add(jscr, BorderLayout.CENTER);

	}

	public JTable getTable() {
		return jtable;
	}

	public AbstractTableModel getModel() {
		return model;
	}

	public void setValueAt(Object val, int row, int col) {
		data[row+1][col+1] = val;
		model.fireTableDataChanged();
	}

	public void setColorAt(Color color, int row, int col) {
		bgCol[row+1][col+1] = color;
		model.fireTableDataChanged();
	}

	public void setFGColorAt(Color color, int row, int col) {
		fgCol[row+1][col+1] = color;
		model.fireTableDataChanged();
	}	

	public void setColumnName(Object val, int col) {
		setValueAt(val, -1, col);
	}

	public void setRowName(Object val, int row) {
		setValueAt(val, row, -1);
	}



	public static void main(String[] args) {

		oscar.visual.VisualFrame frame = new VisualFrame("hello");
		JInternalFrame subframe = frame.createFrame("Table");


		final VisualTable t = new VisualTable(3,4);

		t.setValueAt("toto", 0, 0);
		t.setValueAt("1", 1, 0);
		//subframe.add(b);
		subframe.add(t);
		subframe.pack();
	}


}
