/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 3/10/12
 * Time: 13:13
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing

import oscar.cbls.search.StopWatch
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar, Model}
import oscar.cbls.invariants.lib.logic.{Cluster, IntVar2IntVarFun, Routes}
import oscar.cbls.invariants.lib.set.TakeAny._
import oscar.cbls.invariants.lib.set.TakeAny
import util.Random
import scala.math._

/**supports only a single vehicle*/
object DebugScript extends SearchEngine with App{


  val V:Int = 1


  val random = new Random(0)

  def getRandomDistanceMatrix(N:Int):Array[Array[Int]]= Array.tabulate(N,N)((i,j) => if (i==0 ||j == 0) 0 else random.nextInt(1000)+1)

  def getPlanarDistanceMatrix(coordX:Array[Int],coordY:Array[Int]):Array[Array[Int]] = {
    val N = coordX.length
    Array.tabulate(N,N)((i,j) => round(sqrt((   pow(coordX(i) - coordX(j), 2)
        + pow(coordY(i) - coordY(j), 2) ).toFloat)).toInt)
  }



  //val line = getPlanarDistanceMatrix(Array(1,2,3,4,5,6),Array(0,0,0,0,0,0))
  //val N:Int = 6

  val line2 =  getPlanarDistanceMatrix(Array(1,2,3,4,5,6,-1),Array(0,0,0,0,0,0,0))
  val N2:Int = 7
  var m: Model = new Model(false,true,false,false)
  var vrp = new VRP(N2, 1, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
  vrp.installCostMatrix(line2)
  println("matrix done")
  m.close()
  println("model close")
  //RandomNeighboor(vrp)
  NearestNeighbor(vrp)
  println(vrp.routes)
  println(vrp)
  m.propagate()
  println("Debug route(" + N2 + "points,"+V+"cars)")

  // changement 1 vers 7
  // recherche

  var z = 0
  do{
    m = new Model(false,true,false,false)
    vrp = new VRP(N2, 1, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
    vrp.installCostMatrix(line2)
    m.close()
    //println("Debug route(" + N2 + "points,"+V+"cars)")
    //NearestNeighbor(vrp)
    RandomNeighboor(vrp)
    m.propagate()
    //println(vrp)
    //println(vrp.routes)
    println("start val: " + vrp.ObjectiveVar)

    var nsize = 20
    var saturated = false
    var move:Neighbor = null
    var it = 0
    while(!saturated){
      val oldobj:Int = vrp.ObjectiveVar.value
      //move = OnePointMove.getFirstImprovingMove(vrp,move)
      move = ThreeOptMove.getFirstImprovingMove(vrp,nsize, move)
      if (move != null && move.getObjAfter < oldobj){
        it +=1
        println("it: " + it + " " + move + " " + vrp.ObjectiveVar)
        move.comit
        vrp.ObjectiveVar.value
        println(vrp)
      }else{
        //      if (nsize == 40){
        saturated = true
        //      }else{
        //        nsize = 40
        //        println("GOING TO k=40")
        //      }
      }
    }

    println("done "+ z)


    z+=1
  }
  while(z<100 && vrp.ObjectiveVar.value == 14)



}