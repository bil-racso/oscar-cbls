package scampi.des.examples

import scampi.des.engine._
import scala.util.continuations._
import JSci.maths.statistics._

class Boats {
}

object Boats {
  def main(args: Array[String]) {

    val m = new Model()
    
    val fuelStation = new Resource(m, 2)
    val dock = new Resource(m,3)
    val tank = new Tank(m, 1500)

    class Truck(tankCapacity: Int, name: String) extends Process(m, name) {

      def arrive = {
        println(name + " arrived at time " + m.clock)
        fuelStation.request()
        println(name + " is served at time " + m.clock)
        m.wait(15)
        fuelStation.release()
        println(name + " is leaving at time " + m.clock)
      }
      def firstState = arrive

    }

    class Boat(capacity: Double, name : String) extends Process(m, name){
      def firstState = {
        println ("boat " + name + " arrived")
        dock.request()
        tank.put(capacity)
        dock.release()
        println("boat " + name + " is leaving");
        
      }
      
    }
    
    var i = new Integer(1)
    val boatCapacity = new NumberGenerator(new NormalDistribution(120,250))
    val boatGenerator = new Generator(m, new ExponentialDistribution(90), {
      val t = new Boat(boatCapacity(), "boat:" + i)
      t.run()
      i += 1
    })
    
    
    val truckGenerator = new Generator(m, new ExponentialDistribution(15), {
      val t = new Truck(50, "truck:" + i)
      t.run()
      i += 1
    })

    m.simulate(2000, false)

  }
}