package oscar.invariants


import scala.collection.immutable._
import oscar.invariants._


abstract class Dependency[A](val reactive: Reactive, var reaction: Reaction[A]) extends Depending{ 
  def apply(msg: A) = {
    if ( !reaction.apply(msg) )
      dispose()
  }
  def nowReactsOn(d: Occuring[A]){
    //dispose()
    reaction.dispose()
    reaction = d.foreach(reaction.f)
  }
}


class Reactive {
  val dependingOn = new MyDLL[RDependency[_]]
  
  class RDependency[A](reaction: Reaction[A]) extends Dependency[A](this, reaction){
    val elem = dependingOn.add(this)
    def dispose(){
      reaction.dispose()
      dependingOn.remove(elem)
    }
  }
  def dependsOn[C](d: Occuring[C])(f: C => Boolean): Dependency[C] = {
    new RDependency[C]( for (msg <- d) f(msg))
  }
  def dispose() = {
    for ( d <- dependingOn) d.dispose()
    true
  }
}



class Var[A](_value: A) extends Signal[A](_value) {	
  
  def :=(v: A) = emit (v)
  
  def <= (f: this.type => Any): this.type = {
    f(this)
    this
  }
}

object Var {
  def apply[A](v: A) = {
    v match {
      case Int => new VarInt(v.asInstanceOf[Int])
    }
  }
}

class VarInt(v: Int) extends Var[Int](v) {
  val incChanges = new Event[(Int, Int)]
  @inline override final def :=(v: Int) {
    val old = this()
    super.:=(v)
    incChanges emit (old, v)
  }
  def :+=(v: Int) { this := this() + v}
  def :-=(v: Int) { this := this() - v}
}

class VarList[A]() extends Var[Seq[A]](Nil){
  val isIncreased = new Event[A]
  val isDecreased = new Event[A]
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
  val a = dependsOn(y) { (w: Int) =>
    v := x(w)
    dep nowReactsOn(x(w))
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
    mmap.get(v).get.dispose()
    true
  }
}

object invariants {

  def main(args: Array[String]) {

    val y = new VarInt(1)
    val z = new VarInt(1)
    val x =  (for (j <- 0 to 10) yield new VarInt(100 + j))
    
    
    val s = new VarInt(0) <= sum(z,y)
        
    val l = new VarList[VarInt]
    
    l.add(y)
    l.add(z)

    val sl = new VarInt(0) <= sumOnListOfVars(l)
    
    val f = new VarInt(0) <= (x at y)  //(y, z) //new Element1(x,y)
//
//    when (sl changes){ x => println("sum changed to " + x )
//      true}
//        
//    when(f changes) { w =>
//      println("f changed to " + w)
//      true
//    }

    val r = (y~>{w:Int=>println("y changed "+ w)} | z~>{w:Int=>println("z changed")}) ~> {
      _:Int=> println("y or z just changed")
    } 
    perform (r) until x(6)
    
    
    whenever ( y.filter(_==5) ){w:Int=>
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
