package oscar.distributed


import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.routing.RoundRobinRouter
import akka.util.Duration._
import akka.util.duration._
import oscar.utils.Time._

import akka.util.Timeout

import akka.pattern.ask

import scala.collection.immutable.Stack

class DistributedComputation[R](nbWorkers: Int){
  
  
  implicit val system = ActorSystem("DES")
  // create the master
  //val master = system.actorOf(Props(new Master(nbWorkers)), name = "master")
  
  implicit val timeout = Timeout(5 seconds)
  def apply[A]( block: => A): Future[A] = Future{  block }
    
  def await(results: Traversable[Future[R]]) = Future.sequence(results)
  def reduce(s: Seq[Future[R]])(op: (R,R)=>R) = Future.reduce(s)(op)
  def fold[T](s: Seq[Future[R]])(zero: T)(op: (T,R)=>T) = Future.fold(s)(zero)(op)
}

sealed trait Message
case class Simulate[A](block: () => A) extends Message
case class Done() extends Message
case class AllSent() extends Message
case class Result[A](result: A) extends Message

class SimulationActor extends Actor {

  def receive = {
    case Simulate(block) => {
      block()
      sender ! Done()
    }
  }
}

import scala.collection.immutable.Stack
class Master[R](nrOfWorkers: Int)
  extends Actor {

  val t0 = System.currentTimeMillis()
  var results = new Stack[R]
  var nbSent = 0
  var nbDone = 0
  var nbJobs = 0

  var pi: Double = _
  var nrOfResults: Int = _

  val workerRouter = context.actorOf(
    Props[SimulationActor].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
    
  def receive = {
    case Simulate(block) =>{
      nbSent += 1
      sender ! Result(workerRouter ! Simulate(block))
    }
    case Done() => {
      nbDone += 1
      if ( nbSent == nbDone)
    		println("Done in " + (System.currentTimeMillis() - t0))
    }
    case AllSent() => {
    		  nbJobs = nbSent
      if ( nbSent == nbDone)
    		println("Done in " + (System.currentTimeMillis() - t0))
    		}
    case Result(result: R) =>{
      results = results push(result)
    }
  }
  

}
