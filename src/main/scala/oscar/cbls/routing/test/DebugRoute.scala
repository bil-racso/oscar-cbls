/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 4/10/12
 * Time: 10:10
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing.test

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


object DebugRoute extends App{
  // code debug
  val UNROUTED = 6
  val UNROUTED2 = 12
  var NbPoints = 6
  var NbCars = 1

  var m: Model = new Model(false,true,false,false)
  var Next: Array[IntVar] = Array.tabulate(NbPoints)(i => if(i<NbCars) new IntVar(m, i, NbPoints-1, i, "next" + i)
    else new IntVar(m, 0, NbPoints, i, "next" + i))
   /**
   * Initialisation des variables Next modélisant le cycle actuel
   */
   def init{
    // 0->1->2->3->4->5->0
    Next(0):= 1
    Next(1):= 2
    Next(2):= 3
    Next(3):= 4
    Next(4):= 5
    Next(5):= 0
  }
  init
  var routes = Routes.buildRoutes(Next,NbCars)
  m.close()
  /**
   * Initialisation d'un nouveau modèle
   */
  def initModel(){
    m = new Model(false,true,false,false)
    Next = Array.tabulate(NbPoints)(i => if(i<NbCars) new IntVar(m, i, NbPoints-1, i, "next" + i)
    else new IntVar(m, 0, NbPoints, i, "next" + i))
    init
    routes = Routes.buildRoutes(Next,NbCars)
    m.close()
  }
  def initModel(b:Boolean){
    m = new Model(false,b,false,false)
    Next = Array.tabulate(NbPoints)(i => if(i<NbCars) new IntVar(m, i, NbPoints-1, i, "next" + i)
    else new IntVar(m, 0, NbPoints, i, "next" + i))
    init
    routes = Routes.buildRoutes(Next,NbCars)
    m.close()
  }

  println("Route state 0:"+routes)

  // 3 -> 5 et 4 -> unrouted
  Next(3) := 5
  Next(4) := UNROUTED
  m.propagate()
  println("Route state 1:"+routes)

  // 5 -> 1 et 0 -> unrouted
  Next(5) := 1
  try{
    Next(0) := UNROUTED
    m.propagate() // loop for ever, since a start point has been unrouted. Must be not allowed.
  }
  catch{
    case e:java.lang.AssertionError => println(e.getMessage); initModel
  }
  println("Route state 2:"+routes)

  // 1->3 ; 2 -> unrouted; 3->5; 4 -> unrouted
  Next(1):=3
  Next(2):=UNROUTED
  Next(3):=5
  Next(4):=UNROUTED
  m.propagate()

  println("Route state 3:"+routes)

  initModel(false)
  Next(5) := 1
  Next(1) := 0
  Next(0) := 2
  m.propagate()
  println("Route state 3bis:" + routes)

  initModel
  // swap 1->2 with 4->5
  Next(0):=4
  Next(5):=3
  Next(3):=1
  Next(2):=0
  m.propagate()
  println("Route state 4:"+routes)


  initModel
  // del 4->5 and add 5 after 1
  Next(4):=UNROUTED
  Next(5):=UNROUTED
  Next(3):=0
  Next(1):=5
  Next(5):=2
  m.propagate()
  println("Route state 5:"+routes)


  initModel
  // insert a cycle 3->4 4->3
  try{
    Next(4):=3

    m.propagate()
  }
  catch{
    case e:java.lang.AssertionError => print(e.getMessage); initModel
  }
  println("Route state 6:"+routes)


  initModel
  // insert a temp cycle 3->4 4->3
  try{
    Next(4):=3
    Next(3):=5
    Next(2):=4
    m.propagate()
  }
  catch{
    case e:java.lang.AssertionError => print(e.getMessage); initModel
  }
  println("Route state 7:"+routes)


  initModel
  // insert a temp vehicle unrouted
  try{
    Next(0):=UNROUTED
    Next(4):=1
    Next(5):=2
    Next(0):=5
    m.propagate()
  }
  catch{
    case e:java.lang.AssertionError => print(e.getMessage); initModel
  }
  println("Route state 7-bis:"+routes)





  NbPoints = 12
  NbCars = 2
  m = new Model(false,true,false,false)
  Next = Array.tabulate(NbPoints)(i => if(i<NbCars) new IntVar(m, i, NbPoints-1, i, "next" + i)
    else new IntVar(m, 0, NbPoints, i, "next" + i))

  /**
   * Initialisation des variables Next modélisant le cycle actuel
   */
  def init2{
    // 1->2->3->4->5->6->1
    Next(0):= 2
    Next(2):= 3
    Next(3):= 4
    Next(4):= 5
    Next(5):= 6
    Next(6) :=0

    Next(1):=7
    Next(7):=8
    Next(8):=9
    Next(9):=10
    Next(10):=11
    Next(11):=1
  }
  init2
  routes = Routes.buildRoutes(Next,NbCars)
  println("HERE CHECK \n"+ routes)
  m.close()
  /**
   * Initialisation d'un nouveau modèle2
   */
  def initModel2(){
    m = new Model(false,true,false,false)
    Next = Array.tabulate(NbPoints)(i => if(i<NbCars) new IntVar(m, i, NbPoints-1, i, "next" + i)
    else new IntVar(m, 0, NbPoints, i, "next" + i))
    init2
    routes = Routes.buildRoutes(Next,NbCars)
    m.close()
  }
  // 2 cars now and 2 routes
  println("2 cars and 2 routes:")
  println(routes)

  // move a point to another round done by other car
  // swap 5
  Next(4):=6
  Next(5):= 9
  Next(8):= 5
  m.propagate()
  println("Route state 8:"+routes)


  initModel2
  // swap 3->4 and 8->9
  Next(2):=8
  Next(9):=5
  Next(7):=3
  Next(4):=10
  m.propagate()
  println("Route state 9:"+routes)


  initModel2()
  // change the last elements of both route (6 and 11 by default) with some unrouted node

  Next(6) := 5
  Next(4) := 6
  Next(5) := 0

  Next(8) := 1
  Next(11) := 8
  Next(7) := 11
  Next(9) := UNROUTED2
  Next(10) := UNROUTED2

  m.propagate()
  println("Route state 10:"+routes)

}