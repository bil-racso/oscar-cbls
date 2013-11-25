package oscar.cbls.routing.neighborhood.newneighborhood

import oscar.cbls.routing.model.newVRP.MoveDescription
import oscar.cbls.routing.model.newVRP.VRP
import oscar.cbls.routing.model
import oscar.cbls.modeling.Algebra._
import java.nio.file.OpenOption

abstract class Move(val vrp:VRP with MoveDescription){
  def comit()
  def restartPoint:Int
  def objAfter:Int
}

//c'est toujours le first improve, jamais le best improve.

abstract class neighborhood(){
  def climbAll(s:searchZone, vrp:VRP):Boolean =   //Examine les points une seule fois
    doSearch(s:searchZone, vrp:VRP, 2)
  def climbFirst(s:searchZone):Boolean
  def getFirstImprovingMove(s:searchZone):Option[Move]

  /** effectue la recherche.
    *
    * @param s
    * @param vrp
    * @param searchMode 0: renvoie le mouvement après l'avoir trouvé 1: effectue le premier mouvement et s'arrête 2: effectue tous les mouvements possibles
    *@return
    */
  def doSearch(s:searchZone, vrp:VRP, searchMode:Int):Option[Move]
}

abstract class searchZone(){
  def relevantNeighBor:(Int=> Iterable[Int])
  def nodeIterator:Iterable[Int]
  def vrp:VRP
}

