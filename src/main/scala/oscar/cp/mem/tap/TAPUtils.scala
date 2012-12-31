package oscar.cp.mem.tap

import oscar.cp.modeling._
import oscar.util.selectMin
import oscar.cp.core.CPVarInt

object TAPUtils {

  case class TAPInstance(cargos: Array[Cargo], tanks: Array[Tank], totCapa: Int, incompatibles: Set[(Int, Int)], compatibles: Set[(Int, Int)])

  def parseInstance(filepath: String): TAPInstance = {

    val problemNode = xml.XML.loadFile(filepath)
    val dummyCargo = new Cargo(<cargo id="0" name="empty" volume="0"/>, java.awt.Color.WHITE)
    val cargos = Array(dummyCargo) ++ // dummy cargo
      (for (node <- (problemNode \ "cargos" \ "cargo").toArray)
        yield new Cargo(node))
    val tanks =
      for (node <- (problemNode \ "tanks" \ "tank").toArray)
        yield new Tank(node, cargos)

    val totCapa = (0 /: tanks)((s, t) => s + t.capa) // fold left to compute tot capa

    // extract cargo that cannot be be adjacent to each others
    val incompatibles: Set[(Int, Int)] =
      (for (n <- (problemNode \ "incompatibles" \ "incompatible"))
        yield ((n \ "@cargo1").text.toInt, (n \ "@cargo2").text.toInt)).toSet
    // transform this information to get the possible adjacent pairs
    val compatibles =
      (for (
        i <- 0 until cargos.size;
        j <- 0 until cargos.size;
        if (!incompatibles.contains((i, j)) &&
          !incompatibles.contains((j, i)))
      ) yield (i, j)).toSet

    return new TAPInstance(cargos, tanks, totCapa, incompatibles, compatibles)
  }
}