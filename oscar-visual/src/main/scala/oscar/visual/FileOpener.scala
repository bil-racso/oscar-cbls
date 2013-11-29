package oscar.visual

import java.awt.Component
import java.io.File
import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter;

class FileOpener {
  
  private var currentPath: File = null
  
  def getFile(parent: Component, open: Boolean, extension: String): File = {
    val chooser : JFileChooser = new JFileChooser()
    chooser.setCurrentDirectory(currentPath)
    
    chooser.setFileFilter(new FileFilter() {
      def getDescription: String = extension
      def accept(f: File): Boolean = f.getName.toLowerCase.endsWith(extension) || f.isDirectory
    })
    
    val returnVal = if (open) chooser.showOpenDialog(parent) else chooser.showSaveDialog(parent)
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      var f: File = chooser.getSelectedFile
      if (!f.getName.endsWith(extension)) {
        f = new File(f.getAbsolutePath + "." + extension)
      }
      currentPath = f
      return f
    }
    else {
      return null
    }
  }
}
