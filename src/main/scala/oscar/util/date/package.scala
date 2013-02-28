package oscar.util

import org.scala_tools.time.Imports._
import oscar.util.date._

package object date {

  implicit def int2Date(i: Int) = new Int2Date(i)
  implicit def date2EDate(d: DateTime) = new ExtendedDateTime(d) 
}