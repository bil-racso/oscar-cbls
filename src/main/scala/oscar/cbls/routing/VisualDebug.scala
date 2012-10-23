/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 23/10/12
 * Time: 21:01
 * To change this template use File | Settings | File Templates.
 */


import java.io.File
import javax.swing.JOptionPane
import org.jdesktop.swingx.JXMapViewer
import org.jdesktop.swingx.mapviewer.{WaypointPainter, WaypointRenderer, Waypoint, GeoPosition}
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.visual._
import scala.collection.JavaConversions._
import scala.Double
import scala.io._
import java.awt.{Graphics2D, Dimension, Color}
import util.Random

object VisualDebug extends App{

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
  //obj.pack()


  // towns of belgium
  val path = new File("").getAbsolutePath
  var file = Source.fromFile(path+"\\src\\main\\scala\\oscar\\cbls\\routing\\villesbelgique")
  var townsList:List[(String,Double,Double)] = List.empty
  for(line <- file.getLines()){
    val words = line.split(" ")
    if(words.length == 8)
      townsList = (words(1).toString,words(6).toDouble,words(7).toDouble) :: townsList
  }
  def getRandomTowns(n:Int,towns:List[(String,Double,Double)]):Array[(String,Double,Double)]={
    val townsArray = towns.toArray
    val myTowns:Array[(String,Double,Double)] = new Array(N)
    for(i <- 0 until N){
      val j = new Random().nextInt(townsArray.length)
      myTowns(i) = townsArray(j)
    }
    myTowns
  }

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
    with PositionInRouteAndRouteNr with OptimizeThreeOptWithReverse,
    closeNeighbors:Int, previousMove:Neighbor,moveOp :Int):Neighbor ={
    if(moveOp == 0)
      OnePointMove.getFirstImprovingMove(vrp,previousMove)
    else if(moveOp == 1)
      ThreeOptMove.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
    else if(moveOp == 11)
      ThreeOptOneReverseMove.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
    else
      ThreeOptTwoReverseMove.getFirstImprovingMove(vrp, closeNeighbors, previousMove)
  }

  // model's problem definition

  val V = 1 // nb of vehicles
  val N:Int = 10 // random nb of towns
  val closeNeighbor = 50 // save close neighbors
  println("VRP problem of "+ N + "clients and "+V+ " vehicle(s)")

  val m: Model = new Model(false,false,false,false)
  val vrp = new VRP(N, V, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr
    with ClosestNeighborPoints with OptimizeThreeOptWithReverse

  val arrayTowns = getRandomTowns(N,townsList)
  val locationTowns = arrayTowns.map(t => new Location(t._2,t._3))
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

  def updateVisualisation(ite:Int) {
    def update(i: Int) = lines(i).setDest(locationTowns(vrp.Next(i).value).lat,
      locationTowns(vrp.Next(i).value).lon)
      (0 until N).foreach(update)
      plot.addPoint(ite,vrp.ObjectiveVar.value)
  }

  // strategy's search definition

  //operator ; 1 for OnePointMove ; 2 for ThreeOpt ; 21 for ThreeOpt with 1 flip ; 22 for ThreeOpt with 2 flips
  val op = 21
  val time = 500 // time in millisecond between iterations
  var Neighbor = false
  var previousMove:Neighbor = null
  var ended = false
  var it = 0
  val startObjective = vrp.ObjectiveVar.value
  while(!ended){
    val oldObj:Int = startObjective
    previousMove = getFirstImprovingMove(vrp,closeNeighbor,previousMove,op)
    if (previousMove != null && previousMove.getObjAfter < oldObj){
      it += 1
      previousMove.comit
      updateVisualisation(it)
      println("it: " + it + " | objective: "+ vrp.ObjectiveVar.value + " | move: "+previousMove)
      Thread.sleep(time)
    }
    else ended = true
  }
  println("VRP ended.")
  JOptionPane.showMessageDialog(frame,"Search's strategy is finished. \n" +
    "StartObjective = " +startObjective + "\n" +
    "EndObjective = "+ vrp.ObjectiveVar.value
    )

}