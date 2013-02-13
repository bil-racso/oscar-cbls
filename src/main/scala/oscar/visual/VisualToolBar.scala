package oscar.visual

import javax.swing.JToolBar
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

class VisualToolBar extends JToolBar {

  def addButton(label:String, actionOnClick : => Unit){
    val button = new JButton(label)
    button.addActionListener( new ActionListener() {
		override def actionPerformed(e:ActionEvent) { actionOnClick }
	})
	add(button)
	button
  }
}