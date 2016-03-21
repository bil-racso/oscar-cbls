package oscar.cbls.search
/*
import oscar.cbls.search.core.Neighborhood
import oscar.cbls.search.Benchmark.benchToStatistics
import oscar.cbls.routing.model.VRPObjective
import scala.reflect.io.File

object BenchmarkHelperHelper {

  def benchmark[T <: VRPObjective](problems: Iterable[() => (String, T)], nRuns: Int, strategies: Iterable[T => Neighborhood],
                                   warmupTimeInSeconds: Int = 10, verbose: Int = 0) = {
    val statistics = problems map (problem => {
      val (name, vrp) = problem()
      println("Benchmarking with problem " + name)
      (name, benchToStatistics(vrp.getObjective, nRuns,
        strategies.map(s => () => {
          val strategy = s(vrp)
          (strategy.toString, strategy)
        }), warmupTimeInSeconds,
        verbose))
    })
    println("nRuns = " + nRuns)
    statistics foreach {
      case (problemPath: String, s: Iterable[(String, Benchmark.RunStatistics)]) =>
        val problemName = problemPath.substring(problemPath.lastIndexOf("/") + 1)
        println
        println(problemName)
        println("strategy;it;dur[ms];obj")
        println(statsToCsv(s))
        File("dataSample/csv_results/" + problemName + "_" + System.currentTimeMillis / 1000 + ".csv").writeAll(
          "strategy;it;dur[ms];obj\n" + statsToCsv(s))
    }
  }

  def statsToCsv(stats: Iterable[(String, Benchmark.RunStatistics)], nRuns: Int = 1) = {
    stats.map({
      case (s: String, stats: Benchmark.RunStatistics) =>
        if (nRuns > 1)
          List(s, stats.it.min, stats.duration.min, stats.quality.min).mkString(";")
        else
          List(s, stats.it.min, stats.duration.min, stats.quality.min).mkString(";")
    }).mkString("\n")
  }
}
*/