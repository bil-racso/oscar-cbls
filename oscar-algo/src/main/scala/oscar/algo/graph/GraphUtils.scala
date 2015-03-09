package oscar.algo.graph

import oscar.algo.array.ArrayHeapInt

/**
 * Created on 06/03/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */

/**
 * Helper object to compute various shortest path in directed or
 * undirected graphs
 */
object GraphUtils {
  val UNDEF = -1

  /**
   * This method computes the shortest path from a source to a sink
   * with regards to the edgeCosts matrix using the famous Dijkstra
   * algorithm (http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).
   * These edge costs must be positive otherwise the
   * Bellman–Ford–Moore algorithm must be used.
   *
   * @param source Index representing the source node
   * @param edgeCosts Matrix of the size nodes x nodes where each entry
   *                  (i)(j) represents the cost of the edge going from
   *                  node i to node j. If there is no edge from i to j,
   *                  then the entry must be Int.MaxValue.
   * @return An array of pairs (path, cost) where, for index i, path is,
   *         the succession of index of nodes in the shortest path from
   *         the source to the node i;
   *         cost is the sum of the edge costs along this shortest path
   */
  def dijkstra(source: Int, edgeCosts: Array[Array[Int]]): Array[(Array[Int], Int)] = {
    val n = edgeCosts.length
    val distances = Array.fill(n)(Int.MaxValue)
    val previous = Array.fill(n)(UNDEF)
    distances(source) = 0

    val Q = new ArrayHeapInt(n)
    for (i <- 0 until n) {
      Q.enqueue(i, distances(i))
    }

    while(!Q.isEmpty) {
      val u = Q.dequeue()

      for (v <- 0 until n if u != v) {
        val alt = distances(u) + edgeCosts(u)(v)
        if (alt < distances(v)) {
          Q.changeKey(distances(v), alt, v)
          distances(v) = alt
          previous(v) = u
        }
      }
    }

    def getPath(endPoint: Int): Array[Int] = {
      var path = List[Int]()
      var i = 0
      while(i != source) {
        path +:= i
        i = previous(i)
      }
      path.reverse.toArray
    }

    Array.tabulate(n)(i => (getPath(i), distances(i)))
  }

  /**
   * This method computes the shortest path from a source to a sink
   * with regards to the edgeCosts matrix using the famous Dijkstra
   * algorithm (http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).
   * These edge costs must be positive otherwise the
   * Bellman–Ford–Moore algorithm must be used.
   *
   * @param source Index representing the source node
   * @param edgeCosts Matrix of the size nodes x nodes where each entry
   *                  (i)(j) represents the cost of the edge going from
   *                  node i to node j. If there is no edge from i to j,
   *                  then the entry must be Int.MaxValue.
   * @return An array of pairs (path, cost) where, for index i, path is,
   *         the succession of index of nodes in the shortest path from
   *         the source to the node i;
   *         cost is the sum of the edge costs along this shortest path
   */
  def dijkstra(source: Int, edgeCosts: Array[Array[Double]]): Array[(Array[Int], Int)] = {
    val n = edgeCosts.length
    val distances = Array.fill(n)(Int.MaxValue)
    val previous = Array.fill(n)(UNDEF)
    distances(source) = 0

    val Q = new ArrayHeapInt(n)
    for (i <- 0 until n) {
      Q.enqueue(i, distances(i))
    }

    while(!Q.isEmpty) {
      val u = Q.dequeue()

      for (v <- 0 until n if u != v) {
        val alt = distances(u) + edgeCosts(u)(v)
        if (alt < distances(v)) {
          Q.changeKey(distances(v), alt, v)
          distances(v) = alt
          previous(v) = u
        }
      }
    }

    def getPath(endPoint: Int): Array[Int] = {
      var path = List[Int]()
      var i = 0
      while(i != source) {
        path +:= i
        i = previous(i)
      }
      path.reverse.toArray
    }

    Array.tabulate(n)(i => (getPath(i), distances(i)))
  }
}
