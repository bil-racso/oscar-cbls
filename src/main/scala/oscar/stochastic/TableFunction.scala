package oscar.stochastic

import java.util.TreeMap
import collection.JavaConversions._

trait Operationable[B]{
  implicit def intOp:Operationable[Int] = IntOp
  def +[B1 >: B](a:B1,b:B):B1
  def *[B1 >: B](a:B1,b:B):B1
  def >[B1 >: B](a:B1,b:B):Boolean
  def <[B1 >: B](a:B1,b:B):Boolean
  def zero:B
}

object IntOp extends Operationable[Int]{
  override def +(a:Int,b:Int) = a+b
  override def *(a:Int,b:Int) = a*b
  override def >(a:Int,b:Int) = a>b
  override def <(a:Int,b:Int) = a<b
  override def zero=0
}

class TableFunction[B](mapInit:TreeMap[Int,_<:B]=new TreeMap[Int,B])(implicit val op:Operationable[B]) extends Map[Int,B] with Traversable[(Int,_>:B)] {
  
  val map = new TreeMap[Int,B](mapInit)
  
  override def +[B1 >: B](kv: (Int,B1),op:Operationable[B1] )={
    val r = new TableFunction[B1](map)(op)
    r(kv._1) = kv._2
    r
  }
  override def get(k:Int) = Some(this(k))
  override def apply(i: Int) = if ( map.containsKey(i)) map.get(i) else op.zero
  def domain = map.keySet()
  override def foreach[U]( f: (Int,B) => U ){ map.entrySet().foreach(en=>f(en.getKey(),en.getValue()))}
  
  def update(i: Int, v:B){ map.put(i,v)}
	
  override def toString = map.toString
  
  def +(f2: TableFunction[B]) = new ApplyView2[B](this, f2, op.+)
  def max(f2: TableFunction[B]) = new ApplyView2[B](this, f2, (a,b) => if (op.>(a,b))a else b)
  def *(v: B) = new ApplyView1(this, {x:B => op.*(v,x)} )
  def set(f2: TableFunction[B], f: (B,B) => B ){
    for ( (k,v) <- f2.map.entrySet().map( en => (en.getKey(),f(en.getValue(),this(en.getKey())))) ){
      this(k) = v
    }
  }
  def add(f2:TableFunction[B]) = set(f2,op.+)
  def setMax(f2:TableFunction[B]) = set(f2, (a,b) => if (op.>(a,b))a else b)
}



class ApplyIterator1[B](g: TableFunction[B], f: B => B) extends Iterator[(Int,B)]{
  val iter = g.map.entrySet().iterator()
  def hasNext = iter.hasNext
  def next = {
    val en = iter.next
    (en.getKey(),f(en.getValue()))
  }
}

class ApplyIterator2[B](g: TableFunction[B], h: TableFunction[B], f: (B,B) => B) extends Iterator[(Int,B)]{
  val iterG = g.map.entrySet().iterator()
  val iterH = h.map.entrySet().iterator()
    
  var vnextG: Option[java.util.Map.Entry[Int,B]] = if (iterG.hasNext()) Some(iterG.next()) else None
  var vnextH: Option[java.util.Map.Entry[Int,B]] = if (iterH.hasNext()) Some(iterH.next()) else None
  
  def nextG: Option[java.util.Map.Entry[Int,B]] = {
    val res = vnextG
    vnextG = if (iterG.hasNext()) Some(iterG.next()) else None
    res
  }
  def nextH: Option[java.util.Map.Entry[Int,B]] = {
	val res = vnextH
	vnextH = if (iterH.hasNext()) Some(iterH.next()) else None
	res
  }

  
  def hasNext = vnextG != None || vnextH != None
  
  def next = {
    (vnextG, vnextH) match{
      case(None,Some(a)) => {nextH; (a.getKey(),f(a.getValue(),h.op.zero))}
      case(Some(a),None) => {nextG; (a.getKey(),f(g.op.zero,a.getValue()))}
      case(Some(a),Some(b)) => { 
        if ( a.getKey() < b.getKey()){
          nextG; (a.getKey(),f(a.getValue(),h.op.zero))
        }else if (a.getKey() == b.getKey() ){        
          nextG
          nextH
          (a.getKey(),f(a.getValue(),b.getValue()))
        }else{       
          nextH; (b.getKey(),f(g.op.zero,b.getValue()))
        }          
      }
      case q => throw new RuntimeException(q.toString())
    }
  }
}


class ApplyView1[B](g: TableFunction[B], f: B => B) extends Iterable[(Int,B)]{
  def apply(i: Int) = f(g(i))
  def iterator= new ApplyIterator1[B](g,f)
  override def toString() = {
    var res = "{"
    for ((k,v) <- iterator) res += k + "=" + v + ","
    res += "}"
    res
  }
}

class ApplyView2[B](g: TableFunction[B], h: TableFunction[B], f: (B,B) => B) extends Iterable[(Int,B)]{
  def apply(i: Int) = f(g(i), h(i))
  def iterator= new ApplyIterator2(g,h,f)
}

object TableFunction{
implicit def intOp:Operationable[Int] = IntOp
  
  def apply[B](fb: Iterable[(Int,B)])(implicit op: Operationable[B]): TableFunction[B]={
    val f = new TableFunction[B]
    for ( (k,v) <- fb.iterator ){
      f(k) = v
    }
    f
  }
  def sum[B]( list: Traversable[TableFunction[B]])(implicit op: Operationable[B]) = {
    val res = new TableFunction()
    for ( f <- list ) res add f
    res
  }
  
  implicit def fb2tf[B](fb: Iterable[(Int,B)])(implicit op: Operationable[B]) = this(fb)
  
  def main(args: Array[String]){
    
    implicit def intOp = IntOp
    val f = new TableFunction[Int]
    
    f(2) = 4
    
    println(f);
    
    for ( ((k:Int,v:Int)) <- f){
      println("f(" + k + ") = " + v)
    }
    
    val sum = TableFunction(f+f)
    
    println(sum)
    println( TableFunction(sum max f)*3)
    
  }
}