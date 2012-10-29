package oscar.cbls.routing.visual

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 23/10/12
 * Time: 21:01
 * To change this template use InstanceVRP | Settings | InstanceVRP Templates.
 */


import java.io.File
import java.util.concurrent.Semaphore
import javax.swing.JOptionPane
import org.jdesktop.swingx.JXMapViewer
import org.jdesktop.swingx.mapviewer.{WaypointPainter, WaypointRenderer, Waypoint, GeoPosition}
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing._
import heuristic.{NearestNeighbor, RandomNeighboor}

import neighborhood._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.visual._
import scala.collection.JavaConversions._
import scala.Double
import scala.io._
import java.awt.{Graphics2D, Dimension, Color}
import util.Random
import vrp.Dashboard

object VisualDebug extends App{

  var sem: Semaphore = new Semaphore(0)

  // frame
  val frame = new VisualFrame("VRP")

  // visualize map
  val map = new VisualMap();
  // set preference of map
  map.viewer.setAddressLocation(new GeoPosition(50.8,4.21));
  map.viewer.setZoom(9)
  map.viewer.setPreferredSize(new java.awt.Dimension(800, 800));
  map.viewer.setVisible(true)
  val route = frame.createFrame("Route")
  route.add(map)
  route.pack()


  // visualize objective
  val plot = new Plot2D("","Iteration number","Distance")
  val obj = frame.createFrame("TSP Objective Function")
  //plot.setPreferredSize(new Dimension(400,400))
  obj.add(plot)
  obj.pack()

  //dashboard
  val board = new Dashboard()
  val dashboard = frame.createFrame("Dashboard")
  dashboard.setLocation(800,450)
  dashboard.setPreferredSize(new Dimension(600,400))
  dashboard.add(board)
  dashboard.pack()


  // to redefine color and shape
  val waypointPainter =  new WaypointPainter
  class MyWaypointRenderer extends WaypointRenderer{
    override def paintWaypoint(g:Graphics2D, map:JXMapViewer , wp:Waypoint ) = {
        g.setColor(Color.RED)
        g.fillRect(0,0,10,10)
        g.setColor(Color.BLACK)
        g.fillRect(0,0,10,10)
        true
    }
  }

  waypointPainter.setRenderer(new MyWaypointRenderer())
  //map.viewer.setOverlayPainter(waypointPainter)

  def displayTowns(locationsTowns:Array[Location],vrp:VRP){
    var waypoints:Set[Waypoint] = Set.empty
    locationsTowns.foreach(c => waypoints +=  map.createWaypoint(c.lat,c.lon))
    waypointPainter.setWaypoints(waypoints)
  }

  // get easier the neighbor
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr with SymmetricVRP,
    closeNeighbors:Int, previousMove:Neighbor,moveOp :Int):Neighbor ={
    if(moveOp == 0)
      OnePointMove.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
    else if(moveOp == 1)
      ThreeOptMoveA.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
    else if(moveOp == 11)
      ThreeOptMoveB.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
    else if(moveOp == 12)
      ThreeOptMoveC.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
    else if(moveOp == 2)
      TwoOptMove.getFirstImprovingMove(vrp,closeNeighbor,previousMove)
    else
      SwapMove.getFirstImprovingMove(vrp,closeNeighbor,previousMove)
  }

  // model's problem definition

  val V = 1 // nb of vehicles
  val N:Int = 1000 // random nb of towns
  val closeNeighbor = 30// save close neighbors

  println("VRP problem of "+ N + "clients and "+V+ " vehicle(s)")

  val m: Model = new Model(false,false,false,false)
  val vrp = new VRP(N, V, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr
    with ClosestNeighborPoints with SymmetricVRP with Predecessors

  val arrayTowns = InstanceVRP.random(N)
  val locationTowns = arrayTowns.map(t => new Location(t.long,t.lat))
  displayTowns(locationTowns,vrp) //display feature

  val distMatrix = Array.tabulate(N,N)((i,j) => (locationTowns(i).distance(locationTowns(j))).toInt)
  vrp.installCostMatrix(distMatrix)
  vrp.saveKNearestPoints(closeNeighbor)

  m.close()

  //NearestNeighbor(vrp) // start heuristic
  RandomNeighboor(vrp)
  m.propagate() // propagate heuristic

  // update the visualisation
  val lines = Array.tabulate(N)(i => map.createLine(locationTowns(i).lat,locationTowns(i).lon,
    locationTowns(vrp.Next(i).value).lat,locationTowns(vrp.Next(i).value).lon))

  def getActualRoute(vrp:VRP):String={
    var route = ""
    route += arrayTowns(0).name
    var next = vrp.Next(0).getValue(true)
    while(next != 0){
      route += " -> "+arrayTowns(next).name
      next = vrp.Next(next).getValue(true)
    }
    route
  }


  def updateVisualisation(ite:Int) {
    def update(i: Int) = lines(i).setDest(locationTowns(vrp.Next(i).value).lat,
      locationTowns(vrp.Next(i).value).lon)
      (0 until N).foreach(update)
      plot.addPoint(ite,vrp.ObjectiveVar.value)
      if(board.writeRoute())
        board.updateRouteLabel(getActualRoute(vrp))
  }

  // strategy's search definition

  //operator ; 0 for OnePointMove ; 1 for ThreeOpt ; 11 for ThreeOpt with 1 flip ; 12 for ThreeOpt with 2 flips
  // 2 for twoOpt and 3 for SwapMove
  val op = 12
  val time = 0 // time in millisecond between iterations
  var Neighbor = false
  var previousMove:Neighbor = null
  var ended = false
  var it = 0
  val startObjective = vrp.ObjectiveVar.value

  while(!ended){
    if(board.inPause || board.inIteration()) // visual interface
       board.lock()
    if (it==0)
      updateVisualisation(it)
    val oldObj:Int = startObjective
    previousMove = getFirstImprovingMove(vrp,closeNeighbor,previousMove,op)
    if (previousMove != null && previousMove.getObjAfter < oldObj){
      it += 1
      previousMove.comit
      updateVisualisation(it)
      println("it: " + it + " | objective: "+ vrp.ObjectiveVar.value + " | move: "+previousMove)
      //Thread.sleep(time)
    }
    else ended = true
  }
  println("VRP ended.")
  JOptionPane.showMessageDialog(frame,"Search's strategy is finished. \n" +
    "StartObjective = " +startObjective + "\n" +
    "EndObjective = "+ vrp.ObjectiveVar.value)
  // anypoint unroute ?
  var p = 0
  var next = vrp.Next(p).value
  while(next != 0){
    p += 1
    next = vrp.Next(next).value
   }
  println("Towns routed =" + (p+1))
}