package oscar.examples.visual

import oscar.visual.VisualMap
import oscar.visual.VisualFrame
import oscar.visual.Location
import oscar.visual.Geocoder
import java.io.IOException
import java.awt.Toolkit
import oscar.visual.MapPath
import java.awt.Color
import org.jdesktop.swingx.mapviewer.Waypoint

object DemoMap {
  //coordinates
  val bruxelles = (50.847573, 4.34214)
  val paris = (48.844836, 2.339882)
  val london = (51.51558,-0.14447)
  val rome = (41.8941,12.478821)
  val madrid = (40.388397,-3.726013)
  val luxembourg = (49.61071,6.107574)
  val berlin = (52.549636,13.313782)
  val hamsterdam = (52.362183,4.843323)
  
  val capitals = List(bruxelles, paris, london, rome, madrid, luxembourg, berlin, hamsterdam)
  
  var bp: MapPath = _
  var waypoints = List[Waypoint]()
  
  //display components
  
  val f = new VisualFrame("Map visualization demo");
  val m = new VisualMap();
  val inf = f.createFrame("VisualMap");
  
  
  
  inf.add(m)

  f.pack()

  def runInThread(p: => Unit) = {
    val thread = new Thread(new Runnable {
      def run = p
    })
    thread.start
  }

  def main(args: Array[String]): Unit = {
    val tb = f.createToolBar()

    tb.addButton("run demo", { runInThread(demoMap) })
    tb.addButton("add Bruxelles-Paris", { runInThread(bxlparis) })
    tb.addButton("remove Bruxelles-Paris", { runInThread(removebxlparis) })
    tb.addButton("mark capitals", { runInThread(wp) })
    tb.addButton("unmark capitals", { runInThread(removewp) })
    tb.addButton("clear", { runInThread(clear) })
    f.pack()
    val screensize = Toolkit.getDefaultToolkit().getScreenSize()
    f.setSize((new java.awt.Dimension(screensize.width - 100, screensize.height - 200)))
    inf.setSize((new java.awt.Dimension(screensize.width - 150, screensize.height - 300)))
  }
  
  /*
   * add a green bruxelles to paris path
   */
  def bxlparis = {
    bp = m.createPath(bruxelles, paris, new Color(0, 100, 0))
  }
  
  def removebxlparis = {
    m.removePath(bp)
  }
  
  /*
   * put waypoints on capitals
   */
  def wp = {
    waypoints = capitals.map(p => m.createWaypoint(p._1, p._2))
  }
  
  def removewp = {
    waypoints.foreach(m.removeWaypoint(_))
    waypoints = List[Waypoint]()
  }
  
  /*
   * remove every element from the map
   */
  def clear = {
    m.clear
  }
  
  /*
   * scripted demonstration
   */
  def demoMap = {

    try {
      val citiesNames = List("Namur", "Bruxelles", "Antwerp", "Arlon", "Mons", "Ottignies", "London")
      var citiesCoord = List[Location]()
      for (c: String <- citiesNames) {

        val loc = Geocoder.getLocation(c)
        citiesCoord = citiesCoord :+ loc
        m.createWaypoint(loc.lat, loc.lon);
        try {
          Thread.sleep(200)
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }
      }

      val l = m.createLine((citiesCoord(6).lat, citiesCoord(6).lon), (citiesCoord(1).lat, citiesCoord(1).lon));

      citiesCoord.zipWithIndex.filter(p => p._2 != 1 && p._2 != 6).map(_._1).foreach(lo => m.createPath((lo.lat, lo.lon), (citiesCoord(1).lat, citiesCoord(1).lon)))

    } catch {
      case e1: IOException => e1.printStackTrace()
    }

    try {
      Thread.sleep(2000);
    } catch {
      case e: InterruptedException => e.printStackTrace()
    }

    m.refresh()
  }
}