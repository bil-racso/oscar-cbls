package oscar.cp.mem.tap

import oscar.visual.VisualUtil

class Cargo(node: scala.xml.Node, val color: java.awt.Color = VisualUtil.getRandomColor()) {
  val id = (node \ "@id").text.toInt
  val name = (node \ "@name").text
  val volume = (node \ "@volume").text.toInt
  override def toString = id + ""
}