package oscar.cbls.algo.interval

/**
 * Created by rdl on 10-11-16.
 */
object Interval {

  def smartPrepend(a:Int,b:Int,sortedAggregatedList:List[(Int,Int)]):List[(Int,Int)] = {
    //we suppose that the list is sorted by the first value of the couple, that couples are sorted, and that a is <= to sortedAggregatedList.head._1
    sortedAggregatedList match{
      case Nil =>(a,b) :: sortedAggregatedList
      case (c,d) :: tail =>
        require(a <= c)
        if (b >= c){
          //there is an overlap
          smartPrepend(a,math.max(b,d),tail) //use smartPrepend here because this might overlap with something in tail as well.
        }else{
          //there is no overlap
          (a,b) :: sortedAggregatedList
        }
    }
  }

  def mergeOverlappingIntervals(sortedList:List[(Int,Int)]):List[(Int,Int)] = {
    sortedList match{
      case Nil => Nil
      case (a,b) :: tail => smartPrepend(a,b,mergeOverlappingIntervals(tail))
    }
  }

}
