package oscar.distributed


import akka.actor._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.duration.Duration
import akka.routing.RoundRobinRouter
import oscar.utils.Time._
import ExecutionContext.Implicits.global
import akka.util.Timeout

import akka.pattern.ask

import scala.collection.immutable.Stack

object DistributedComputation{
  def apply[I,R](block: I => R) = new DistributedComputation(block)
}
class DistributedComputation[I,R](block: I=> R){
    
  implicit val system = ActorSystem.apply()
  // create the master
  //val master = system.actorOf(Props(new Master(nbWorkers)), name = "master")
  
  //implicit val timeout = Timeout(5 seconds)
  def apply(i: I) = Future{ block(i) }
  
  def run(iter: Iterable[I]) = for (i <- iter) yield{this(i)}
  def runAndReduce(iter: Iterable[I])(op: (R,R)=>R) = reduce(run(iter))(op)
  def await(results: Traversable[Future[R]]) = Future.sequence(results)
  def reduce(s: Iterable[Future[R]])(op: (R,R)=>R) = Future.reduce(s)(op)
  def fold[T](s: Seq[Future[R]])(zero: T)(op: (T,R)=>T) = Future.fold(s)(zero)(op)
  def foreach(s: Seq[Future[R]])(op: R=>Unit) = Future.fold(s)(Unit){(z,r) => 
    op(r)
    Unit
  }
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
