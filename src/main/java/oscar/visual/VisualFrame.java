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

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class VisualFrame extends JFrame {


	JDesktopPane desktop;
	Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	
	private int w;
	private int h;
	private int n;
	private int nbLines;
	private int nbCols;
	
	
	public VisualFrame(String title,int nbLines, int nbCols) {
		super(title);
		this.nbLines = nbLines;
		this.nbCols = nbCols;
		n = 0;
		w = screenSize.width/nbCols;
		h = screenSize.height/nbLines;

		addWindowListener(new WindowAdapter() {
			 public void windowClosing(WindowEvent event) {
				    System.exit(0);
			  }
		});
		
		Container content = getContentPane();
		content.setBackground(Color.white);
		desktop = new JDesktopPane();
		desktop.setBackground(Color.white);
		content.add(new JScrollPane(desktop));	
		setSize(screenSize);
		setVisible(true);
	}
	
	public VisualFrame(String title) {
		this(title,2,2);

	}
	
	
	
	protected JMenuBar createMenuBar() {
		JMenuBar menuBar = new JMenuBar();
		JMenu menu = new JMenu("Frame");
		menu.setMnemonic(KeyEvent.VK_N);
		JMenuItem menuItem = new JMenuItem("New IFrame");
		menuItem.setMnemonic(KeyEvent.VK_N);
		menuItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				createFrame("sub frame");
			}
		});
		menu.add(menuItem);
		menuBar.add(menu);
		return menuBar;
	}
	
	public JInternalFrame createFrame(String title) {
		int c = n%nbCols;
		int l = n/nbCols;
		
		JInternalFrame frame = new JInternalFrame(title, true, false, true, true);
		
		frame.setLocation(c*w,l*h);
		frame.setSize(w,h);
		frame.setBackground(Color.white);
		frame.setVisible(true);
		desktop.add(frame);
		frame.moveToFront();
		n++;
		return frame;
	}	


	public static void main(String[] args) {
		VisualFrame frame = new VisualFrame("My Frame");
		JInternalFrame subframe = frame.createFrame("My Sub-frame");
	}

}
