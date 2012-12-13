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

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.test
import javax.swing.JOptionPane
import org.jdesktop.swingx.mapviewer.{Waypoint, GeoPosition}
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing.model._
import oscar.cbls.routing._
import initialSolution.{NearestNeighbor}
import neighborhood._
import oscar.visual._
import java.awt.Dimension
import visual.Dashboard

/**
 * Don't work anymore. New visual applet is available in package Visual.
 */

/*


object OldVisualMap extends App{

  // frame
  val frame = new VisualFrame("VRP")

  // visualize map
  val map = new VisualMap();
  map.viewer.setAddressLocation(new GeoPosition(50.8,4.21));
  map.viewer.setZoom(9)
  map.viewer.setPreferredSize(new java.awt.Dimension(800, 800));
  map.viewer.setVisible(true)

  val route = frame.createFrame("Route")
  route.add(map)
  route.pack()

  // visualize objective
  var plot = new Plot2D("","Iteration number","Distance")
  val obj = frame.createFrame("TSP Objective Function")
  //plot.setPreferredSize(new Dimension(400,400))
  obj.add(plot)
  obj.pack()
  def initPlot(){
    plot.getPoints().clear()
  }

  //dashboard
  val board = new Dashboard(false)
  val dashboard = frame.createFrame("Dashboard")
  dashboard.setLocation(800,450)
  dashboard.setPreferredSize(new Dimension(600,400))
  dashboard.add(board)
  dashboard.pack()


  // model's problem definition

  var V:Int = 0 // nb of vehicles
  var N:Int = 0 // random nb of towns
  var closeNeighbor:Int =  0// save close neighbors
  var m: Model = null
  var vrp: VRP with HopDistanceAsObjective with PositionInRouteAndRouteNr
    with ClosestNeighborPoints with SymmetricVRP with Predecessors with Unrouted = null
  var arrayTowns:Array[Point] = null
  var wayPoints :Array[Location] = null
  var lines :Array[MapLine]= null
  var distMatrix :Array[Array[Int]]= null

  // Methods helpers

  //close the Next array in case of new test on the same instance
  var clonedNext:Array[Int] = null
  def cloneHeuristic() {
    clonedNext = new Array[Int](N)
    for(i <- 0 until N)
      clonedNext(i) = vrp.Next(i).value
  }


  // display waypoints on map
  def displayTowns(){
    var waypoints:Set[Waypoint] = Set.empty
    wayPoints.foreach(c => waypoints +=  map.createWaypoint(c.lat,c.lon))
    //waypointPainter.setWaypoints(waypoints)
  }

  // display lines between waypoints
  def displayLines() {
    lines = Array.tabulate(N)(i => map.createLine(wayPoints(i).lat,wayPoints(i).lon,
      wayPoints(vrp.Next(i).value).lat,wayPoints(vrp.Next(i).value).lon))
  }

  // return actual route in a String format
  def actualRoute(vrp:VRP):String={
    var route = ""
    route += arrayTowns(0).name
    var next = vrp.Next(0).getValue(true)
    while(next != 0){
      route += " -> "+arrayTowns(next).name
      next = vrp.Next(next).getValue(true)
    }
    route
  }

  // get easier the neighborhood selected
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr with SymmetricVRP with Unrouted ,
                            closeNeighbors:Int, previousMove:Neighbor):Neighbor = {
    board.neighborhood.getSelectedIndex match{
      case 0 => OnePointMove.getFirstImprovingMove(vrp, vrp.getKNearest(closeNeighbors), previousMove)
      case 1 => ReinsertPoint.getBestMove(vrp)
      case 2 => RemovePoint.getBestMove(vrp)
      case 3 => Swap.getFirstImprovingMove(vrp,vrp.getKNearest(closeNeighbors),previousMove)
      case 4 => ThreeOptA.getFirstImprovingMove(vrp, vrp.getKNearest(closeNeighbors), previousMove)
      case 5 => ThreeOptB.getFirstImprovingMove(vrp, vrp.getKNearest(closeNeighbors), previousMove)
      case 6 => ThreeOptC.getFirstImprovingMove(vrp, vrp.getKNearest(closeNeighbors), previousMove)
      case 7 => TwoOpt.getFirstImprovingMove(vrp,vrp.getKNearest(closeNeighbors),previousMove)
      case _ => null
    }
  }

  // initialize the model with dashboard's parameters
  def initModel(reset:Boolean=false){
    V = board.nbVehicle.getText.toInt
    N = board.nbNodes.getText.toInt
    closeNeighbor = board.klimited.getText.toInt
    m = new Model(false,false,false,false)
    vrp = new VRP(N, V, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr
      with ClosestNeighborPoints with SymmetricVRP with Predecessors with Unrouted
    try{
      arrayTowns = {
        board.instances.getSelectedIndex match {
          case 0 => BelgiumInstance.random(N)
          case 1 => BelgiumInstance.instance_1(N)
          case 2 => BelgiumInstance.instance_2(N)
          case 3 => BelgiumInstance.instance_3(N)
          case 4 => BelgiumInstance.instance_4(N)
          case 5 => BelgiumInstance.instance_5(N)
        }}}
    catch {
      case e:AssertionError => println("Number of towns is too big.")
    }
    wayPoints = arrayTowns.map(t => new Location(t.long,t.lat))
    distMatrix = Array.tabulate(N,N)((i,j) => (wayPoints(i).distance(wayPoints(j))).toInt)
    vrp.installCostMatrix(distMatrix)
    vrp.saveKNearestPoints(closeNeighbor)
    m.close()
    if (reset)
      for (i <- 0 until clonedNext.length)
        vrp.Next(i) := clonedNext(i)
    else{
      board.heuristic.getSelectedIndex match {
       // case 0 => RandomNeighbor(vrp)
        case 1 => NearestNeighbor(vrp)
      }
    }
    m.propagate()
    cloneHeuristic
    map.waypoints.clear()
    map.lines.clear()
    displayTowns()
    displayLines()
    it = 0
    initPlot
    println(vrp.routes)
  }



  // update the visualisation while strategy's search
  def updateVisualisation(ite:Int) {
    def update(i: Int) {
      if(vrp.Next(i).value != N){
        lines(i).setDest(wayPoints(vrp.Next(i).value).lat,
        wayPoints(vrp.Next(i).value).lon)
      }
      else{
        lines(i).setDest(wayPoints(i).lat,wayPoints(i).lon)
      }
    }

    for (i <- 0 until N) update(i)
    plot.addPoint(ite,vrp.ObjectiveVar.value)
    if(board.writeRoute())
      board.updateRouteLabel(actualRoute(vrp))
  }
  //initModel

  // strategy's search definition, start the search.
  var it = 0
  class Search extends Runnable{
    def run(){
      var previousMove:Neighbor = null
      var ended = false
      val startObjective = vrp.ObjectiveVar.value
      while(!ended){
        if(board.inPause || board.inIteration()) // visual interface
           board.lock()
        val oldObj:Int = vrp.ObjectiveVar.value
        previousMove = getFirstImprovingMove(vrp,closeNeighbor,previousMove)
        if ((previousMove != null && previousMove.getObjAfter < oldObj) || previousMove.isInstanceOf[ReinsertPoint]){
          it += 1
          previousMove.comit
          updateVisualisation(it)
          println("it: " + it + " | objective: "+ vrp.ObjectiveVar.value + " | move: "+previousMove +"\n M(ax)A(vg)U(nrouted)N(eighbor) ="+
          vrp.maxAvgUnrouted)
        }
        else ended = true
      }
      println("VRP ended.")
      JOptionPane.showMessageDialog(frame,"Search's strategy is finished. \n" +
      "StartObjective = "+startObjective+
      "EndObjective = "+ vrp.ObjectiveVar.value)

      board.firstIte = true
      board.pause = true
      board.start.setText("Start")
    }
  }
}

*/