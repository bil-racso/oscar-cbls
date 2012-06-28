/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.visual;


import java.awt.Color;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.LinkedList;

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


public class VisualDrawing extends JPanel  {

	JPanel drawingPanel;


	LinkedList<ColoredShape> shapes;
	
	public VisualDrawing(boolean saveButton) {
		this(saveButton,false);
	}

	public VisualDrawing(boolean saveButton, final boolean flipped) {
		super(new BorderLayout());
		shapes = new LinkedList<ColoredShape>();
		
		drawingPanel = new JPanel() {
			
			public void paintComponent(Graphics g) {
				
				if (flipped) {
					g.translate(0,getHeight()); 
					((Graphics2D)g).scale(1, -1);
				}
				super.paintComponent(g);
				for (ColoredShape s: shapes) {
					s.draw((Graphics2D)g);
				}
			}
		};
		
		drawingPanel.addMouseMotionListener(new MouseMotionListener() {
			
			@Override
			public void mouseMoved(MouseEvent e) {
				drawingPanel.setToolTipText("");
				for (ColoredShape s: shapes) {
					s.showToolTip(e.getPoint());
				}
			}
			@Override
			public void mouseDragged(MouseEvent arg0) {
			}
		});
		
		
		drawingPanel.setBackground(Color.white);

		add(drawingPanel, BorderLayout.CENTER);


		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		
		buttonPanel.setBackground(Color.white);


	}
	
	public void showToolTip(String text) {
		drawingPanel.setToolTipText(text);
	}


	public void addShape(ColoredShape s) {
		shapes.add(s);
		repaint();
	}
	
	
	
	public static void main(String[] args) {
		
		VisualFrame f = new VisualFrame("toto");
		VisualDrawing d = new VisualDrawing(false);
		JInternalFrame inf = f.createFrame("Drawing");
		inf.add(d);
		f.pack();
		Rectangle2D r = new Rectangle2D.Double(0, 0,100,100);
		ColoredShape<Rectangle2D> rect = new ColoredShape<Rectangle2D>(d,r);

	    ColoredShape<Line2D> l = new ColoredShape<Line2D>(d,new Line2D.Double(0, 0, 100, 100));
		
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		rect.setInnerCol(Color.red);
		
		
		
	}






}
