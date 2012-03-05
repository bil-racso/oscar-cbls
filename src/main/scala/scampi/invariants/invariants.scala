package scampi.invariants


import scala.collection.immutable._
import scampi.invariants._


abstract class Dependency[A](val reactive: Reactive, f: A => Boolean) extends ListDepending[A](f){ 
  def nowDependsOn(d: Occuring[_,A])
}


class Reactive {
  val dependingOn = new MyDLL[RDependency[_]]
  
  class RDependency[B](m: EventModel, dinit: Occuring[_,B], f: B => Boolean) extends Dependency[B](this, f){
    def model = m
    var reaction = dinit.foreach(f)
    val elem = dependingOn.add(this)
    def dispose(){
      reaction.dispose()
      dependingOn.remove(elem)
    }
    override def nowDependsOn(d: Occuring[_,B]){
      reaction.dispose()
      reaction = d.foreach(f)
    }
  }
  def dependsOn[B](d: Occuring[_,B])(f: B => Boolean): Dependency[B] = new RDependency[B](d.model, d,f)
  def dispose() = {
    for ( d <- dependingOn) d.dispose()
    true
  }
}



class Var[A](m: EventModel, _value: A) extends SourceSignal[A](m, _value) {	
  
  def :=(v: A) = set(v)
  
  def <= (f: this.type => Any): this.type = {
    f(this)
    this
  }
}

class VarInt(m: EventModel, v: Int) extends Var[Int](m, v) {
  val incChanges = new Event[(Int, Int)](m)
  @inline override final def :=(v: Int) {
    val old = this()
    super.:=(v)
    incChanges emit (old, v)
  }
  def :+=(v: Int) { this := this() + v}
  def :-=(v: Int) { this := this() - v}
}

class VarList[A](m: EventModel) extends Var[Seq[A]](m, Nil){
  val isIncreased = new Event[A](m)
  val isDecreased = new Event[A](m)
  def add(elem: A){
    this := this() :+ elem
    isIncreased emit(elem)
  }
  def remove(elem: A){
    this := this().drop(this().indexOf(elem))
    isDecreased emit(elem)    
  }
}

abstract class StaticInvariant[R] extends Reactive {


}

class ElementArray[A](arr: scala.collection.immutable.IndexedSeq[Var[A]]) {
  @inline def at(y: Var[Int]) = {v: Var[A] =>
    new Element1(arr, y, v)
  }
}
//class ElementArray2[A](arr: IndexedSeq[IndexedSeq[Var[A]]]) {
//  def at(y: Var[Int], z: Var[Int]) = {
//    new Element2(arr, y, z)
//  }
//}

class Element1[A](x: IndexedSeq[Var[A]], y: Var[Int], v: Var[A]) extends StaticInvariant[A] {
  v := x(y)
  def scope() = y +: ( for (v <- x) yield v)
  var dep = dependsOn(x(y)) { (w: A) =>
    v := w
    true
  }
  val a = dependsOn(y.changes) { (w: Int) =>
    v := x(w)
    dep nowDependsOn(x(w))
    true
  }
}


class SumInvariant(result: VarInt, list: List[VarInt]) extends StaticInvariant[Int] {
  def scope = (for ( v <- list ) yield v.incChanges).toIndexedSeq
  var a = 0
  for ( v <- list.iterator ){
    a += v
    dependsOn (v incChanges){ case (o,n) =>
      result :+= n-o     
      true
    }
  }
  result := a
}

class SumInvariantOnList(result: VarInt, list: VarList[Int]) extends StaticInvariant[Int] {
  var a = 0
  for ( v <- list().iterator ){
    a += v
  }
  result := a
  dependsOn (list.isIncreased ){ w=>
    result :+= w
    true
  }
}


class SumInvariantOnListOfVars(result: VarInt, list: VarList[VarInt]) extends StaticInvariant[Int] {
  var a = 0
  val mmap = new scala.collection.mutable.HashMap[VarInt,Dependency[(Int,Int)]]
  for ( v <- list().iterator ){
    mmap.put(v, dependsOn(v.incChanges){ case(o,n) =>
    	result :+= n-o
    	true
    })
    a += v
  }
  result := a
  dependsOn (list.isIncreased ){ v=>
    result :+= v
    mmap.put(v, dependsOn(v.incChanges){ case(o,n) =>
    	result :+= n-o
    	true
    })
    true
  }
  dependsOn (list.isDecreased ){ v=>
    result :-= v
    mmap.get(v) match{
      case None => println("no such variable in list")
      case Some(v: Dependency[_]) => v.dispose()
    }
    true
  }
}

object invariants {

  def main(args: Array[String]) {

    val m = new EventModel
    
    val y = new VarInt(m, 1)
    val z = new VarInt(m, 1)
    val x =  (for (j <- 0 to 10) yield new VarInt(m, 100 + j))
    
    
    val s = new VarInt(m, 0) <= sum(z,y)
        
    val l = new VarList[VarInt](m)
    
    l.add(y)
    l.add(z)

    val sl = new VarInt(m, 0) <= sumOnListOfVars(l)
    
    val f = new VarInt(m, 0) <= (x at y)  //(y, z) //new Element1(x,y)
//
    when (sl changes){ x => println("sum changed to " + x )
      true}
//        
    when(f changes) { w =>
      println("f changed to " + w)
      true
    }

//    val r = (y~~>{w:Int=>println("y changed "+ w)} | z~~>{w:Int=>println("z changed")}) ~~> {
//      _: Unit=> println("y or z just changed")
//    } 
//    perform (r) until x(6)
    
    when(y){
      println(111)
      true}
    when(y){println(222)
      true}
    whenever ( y ){
      println("super")
    }
     
    for ( msg <- y; if ( msg==5 ) ){
      println(msg)
      true
    }
    
    y := 5
    y := 4
    x(3) := 5
    x(4) := 10
    y := 2
    z := 5
    println("-------")
    x(6) := 87
    y := 7
    z := 79
    l.add(x(1))
    l.remove(y)
    y := 5
  }
}