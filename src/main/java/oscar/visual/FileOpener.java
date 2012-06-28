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

import java.awt.Component;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.filechooser.FileFilter;

public class FileOpener {


		File currentPath;


		public File getFile(Component parent, boolean open, final String extention) {
			JFileChooser chooser = new JFileChooser();
			chooser.setCurrentDirectory(currentPath);
			
			chooser.setFileFilter(new FileFilter() {
				public String getDescription() {
					return extention;
				}
				public boolean accept(File f) {
					return f.getName().toLowerCase().endsWith(extention) || f.isDirectory();
				}
			});
			int returnVal = open ? chooser.showOpenDialog(parent) : chooser.showSaveDialog(parent);
			if(returnVal == JFileChooser.APPROVE_OPTION) {
				File f =  chooser.getSelectedFile();
				if (!f.getName().endsWith(extention)) {
					f = new File(f.getAbsolutePath()+"."+extention);
				}
				currentPath = f;
				return f;
			} else {
				return null;
			}
		}

}
