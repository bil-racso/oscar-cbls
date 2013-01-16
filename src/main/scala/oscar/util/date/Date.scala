package oscar.util.date
//
//import org.scala_tools.time.Imports._
//
//class Date extends java.util.Date(0) {
//
//  private val cal = new java.util.GregorianCalendar()
//  def set(field: Int, v: Int){cal.set(field,v)}
//  def at(h:Int)={
//    cal.set(HOUR,h)
//    this
//  }  
//  def th(y: Int)={
//    cal.set(YEAR,y)
//    this
//  }
//  def pm(m: Int)={
//	  cal.set(MINUTE,m)
//	  cal.set(AM_PM,PM)
//	  this
//  }
//  def am(m: Int)={
//	  cal.set(MINUTE,m)
//	  cal.set(AM_PM,AM)
//	  this
//  }
//}
////
////object Month(index: Int) {
////  def apply(d: Int) = {
////    
////  }
////}
//
//object January{
//  val index = 1
//   def /(d: Int) = {
//     val res = new Date
//     res.set(MONTH, index)
//     res.set(DAY_OF_MONTH, d)
//     res
//   }
//  def main(){
//    val d = January/5 th 2012 at 10 pm 20
//  }
//}