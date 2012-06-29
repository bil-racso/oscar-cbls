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
