package scampi.invariants

import scala.collection.immutable._
import scala.util.continuations._

class EventModel{
  val pendings = new scala.collection.mutable.Queue[ListDepending[_]]
  private var delay = false
  def pend(r: ListDepending[_]){
    pendings += r
    if ( !delay ) processPendings
  }
  def processPendings{
    while ( !pendings.isEmpty) pendings.dequeue.applyNext
  }
  def withDelay(block: => Unit){
    delay = true
    block
    delay = false
    processPendings
  }
}

abstract class Depending[A](val f: A => Boolean){
  def dispose()
  def model: EventModel
}

class NullReaction[A](m: EventModel) extends Depending[A]({(x:A)=>false} ){
  def dispose(){}
  def model = m
}

abstract class ListDepending[A](f: A => Boolean) extends Depending[A](f){
  val pendings = new scala.collection.mutable.Queue[A]
  def execute(msg: A): Boolean = f(msg)
  
  def apply(msg: A) = {
    pendings += msg
    model.pend(this)
  }
  def until(d: Occuring[_,_]) {
    once(d) { dispose() }
  }
  def applyNext{
    if (!execute(pendings.dequeue)){
    	dispose()
    	false
    }else
      true
  }
}

trait Occuring[A,B] {
  
  def model: EventModel
  def apply( f: B => Unit ) = this ~~> f
  def ~~>( f: B => Unit ) = new ReactionDescription(model,this, {msg:B=>f(msg);true})
  def foreach( f: B => Boolean): Depending[A]

  def ===(a: B): Occuring[A,Unit]
  
}

class MapOccuring[A,B,C](m: EventModel, d: BaseEvent[A,B], fmap: B => C) extends BaseEvent[A,C]{
  def model = m
  def foreach(f: C => Boolean) = d.foreach(x=> f(fmap(x)))
}


class ConditionalOccuring[A,B](m: EventModel, d: BaseEvent[A,B], f:B=>Boolean) extends BaseEvent[A,B]{
  def model = m
  def foreach(f2: B => Boolean)={
    for ( msg <- d ){
      if ( f(msg) ){
        f2(msg)
      }else{
        true
      }
    }
  }
}

trait BaseEvent[A,B] extends Occuring[A,B]{
  
	def map[C](f: B => C): BaseEvent[A,C] = new MapOccuring(model, this, f)
  def ===(a: B): Occuring[A,Unit] = filter(_ == a).map()
  def is(a:B) = ===(a)
  def filter(f: B => Boolean): BaseEvent[A,B] = new ConditionalOccuring(model, this,f)
}

trait SourceEvent[A] extends BaseEvent[A,A]{
	class EventReaction(var event: SourceEvent[A], f: A => Boolean) extends ListDepending[A](f)
	 {
		val cf = dependants.add(this)
		def model = event.model
		def dispose(){
			dependants.remove(cf)
		}
	}
	protected val dependants = new MyDLL[EventReaction]
	override def foreach(f: A => Boolean) = {
	  val r = new EventReaction(this,f)
	  r
	}
	
	def hanging = dependants.size
	def emit(msg:A)
}


class OrEvent(m: EventModel) extends Reactive with NotifyAllEvent[(String,Any)]{
  def model = m
  var i = 0
  def |[A](eb: ReactionDescription[A]) ={
    val j = i
    dependsOn(eb.e){ msg: A =>
      eb.f(msg)
      this emit ((eb.name,msg))
      dispose()
    }
    i += 1
    this
  }
}

class AndEvent(m: EventModel) extends Reactive with NotifyAllEvent[Unit]{
  def model = m
  var n = 0
  var tot = 0
  def &[A](eb: ReactionDescription[A])= {
    val j = tot
    dependsOn(eb.occuring){ msg: A =>
      n += 1
      once (this) { _ => eb.f(msg)}
      if ( n == tot ) this emit ()
      false
    }
    tot += 1
    this
  }
}


class ReactionDescription[A](model: EventModel, val e: Occuring[_,A], val f: A => Boolean) {
  var name = ""
  def ::(n: String)={
    name = n
    this
  }
  def occuring = e
  def | (eb: ReactionDescription[_]) = {
    val res = new OrEvent(model)
    res | this | eb
    res
  }
  def & (eb: ReactionDescription[_]) = {
    val res = new AndEvent(model)
    res & this & eb
    res
  }  
  
  def post() = occuring.foreach(f)
}

class SourceSignal[A](m: EventModel, v: A) extends Signal[A,A](v){
  def model = m
  val changes = new Event[A](m)
  def set(v: A) = {
    if ( value != v){
      value = v
      changes emit(v)
    }
  }
  override def foreach(f: A=>Boolean) = {
    if ( f(value) ){
    	for ( msg <- changes){
    	    f(msg) 
    	}
    }else
      new NullReaction(changes.model)
  }
}

abstract class Signal[A,B](var value: B) extends Occuring[A,B]{
  def model: EventModel
  def m = model
  def apply() = value
  def map[C](f: B=>C): Signal[A,C] = new MapSignal(model, this, f)
  
  def filter(f: B => Boolean): Signal[A,Option[B]] = map{x =>
    if (f(x)) Some(x)
    else None
  }
  class eeeSignal(v: B) extends Signal[A,Unit]{
    def model = m
    def foreach(f: Unit => Boolean) = {
      filter(_==v).foreach{msg =>
        if ( msg==None) true
        else f()
      }
    }
  }
  def is (v: B) = value == v
  def ==(that: Signal[_,B]) = this()==that()
  
  override def ===(a: B): Occuring[A,Unit] = //new eeeSignal(a)
    filter(_==a).map{x:Option[B] => 
      if ( x==None) None
      else Some()
    }
	
}

class MapSignal[A,B,C](m: EventModel, d: Signal[A,B], fmap: B => C) extends Signal[A,C](fmap(d())){
  def model = m
  def foreach(f: C => Boolean) = d.foreach(x=> f(fmap(x)))
}



trait NotifyAllEvent[A] extends SourceEvent[A]{  
  def emit(msg: A) {
    for (d <- dependants) d(msg)
  }
}

trait NotifyOneEvent[A] extends SourceEvent[A]{
  def emit(a: A){
    if ( dependants.first != null ) dependants.first.apply(a)
  }
}


