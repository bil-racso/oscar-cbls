package oscar.des


package object engine {
	import akka.util.Duration
	import akka.util.duration._
	import org.joda.time._
	implicit def now(implicit m: Model[_]): DateTime = m.clock()
	implicit def duration2Long(d: Duration): Long = d.toMillis
	implicit def date2Long(d: java.util.Date): Long = d.getTime()
	implicit def long2Date(l: Long) = new java.util.Date(l)
    val weeks = 7 days
}