package oscar.cp.mem.tap

class Tank(node: scala.xml.Node, cargos: Array[Cargo]) {
  val id = (node \ "@id").text.toInt
  val capa = (node \ "@capa").text.toInt
  val x = (node \ "@x").text.toInt
  val y = (node \ "@y").text.toInt
  val w = (node \ "@w").text.toInt
  val h = (node \ "@h").text.toInt
  val impossibleCargos =
    for (n <- (node \ "impossiblecargos" \ "cargo").toArray)
      yield (n \ "@id").text.toInt
  val neighbours =
    for (n <- (node \ "neighbours" \ "tank").toArray)
      yield (n \ "@id").text.toInt
  val possibleCargos = (0 until cargos.size).filter(!impossibleCargos.contains(_)).toSet
}