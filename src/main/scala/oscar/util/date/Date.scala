package oscar.util.date
import org.scala_tools.time.Imports._

class ExtendedDateTime(val d: DateTime) extends AnyVal {

  def at(h:Int)=d.withHour(h)
  def h(m: Int)=d.withMinuteOfHour(m)
  def m(s: Int)=d.withSecondOfMinute(s)
  def and(s: Int)=m(s)
  def seconds(m: Int)=d.withMillisOfSecond(m)
  def second(m: Int)=d.withMillisOfSecond(m)
  def seconds=d
  def second=d
}

class Int2Date(val i: Int) extends AnyVal{
  def january(y: Int) = new DateTime(y,1,i,0,0,0)
  def february(y: Int) = new DateTime(y,2,i,0,0,0)
  def march(y: Int) = new DateTime(y,3,i,0,0,0)
  def april(y: Int) = new DateTime(y,4,i,0,0,0)
  def may(y: Int) = new DateTime(y,5,i,0,0,0)
  def june(y: Int) = new DateTime(y,6,i,0,0,0)
  def july(y: Int) = new DateTime(y,7,i,0,0,0)
  def august(y: Int) = new DateTime(y,8,i,0,0,0)
  def september(y: Int) = new DateTime(y,9,i,0,0,0)
  def october(y: Int) = new DateTime(y,10,i,0,0,0)
  def november(y: Int) = new DateTime(y,11,i,0,0,0)
  def december(y: Int) = new DateTime(y,12,i,0,0,0)
}


object Date{
   
  def apply(y: Int, m: Int, d: Int) = new DateTime(y,m,d,0,0,0)
  def main(args: Array[String]){
    val d = Date(1970,1,2)
    println( d at 8 )
    println(5 january 2012) 
    println(5 january 2012 at 8 h 30 and 5 seconds 547)  
    println(5 january 2012 at 8 h 30 m 5 seconds 547)  
    println(5 january 2012 at 8 h 30 and 5 seconds)  
  }
}