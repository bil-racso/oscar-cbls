package oscar.cbls.routing.visual

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.BoxPanel
import scala.swing.Orientation
import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.JFrame._

object VRPUI extends JFrame {
  
  def main(args :Array[String]) {
        var win = new JFrame("VRP Test");

        win.setSize(800,600)
        win.add(new PanelVRP(false))
        win.setDefaultCloseOperation(EXIT_ON_CLOSE)
        win.setVisible(true)
  }
  
}
