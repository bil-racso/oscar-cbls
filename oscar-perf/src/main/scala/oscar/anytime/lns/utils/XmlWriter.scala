package oscar.anytime.lns.utils

import oscar.cp.searches.lns.CPIntSol

import scala.xml.{Elem, NodeBuffer, PrettyPrinter}


object XmlWriter{

  private def resultsToXml(
                            config: String,
                            timeout: Long,
                            instance: String,
                            problem: String,
                            bestKnown: Int,
                            maxObjective: Boolean,
                            solutions: Iterable[CPIntSol]
                          ): Elem = {

    val xml = new NodeBuffer()

    xml += <config>{config}</config>
    xml += <timeout>{timeout}</timeout>
    xml += <instance>{instance}</instance>
    xml += <problem>{problem}</problem>
    xml += <objective_type>{if(maxObjective) "max" else "min"}</objective_type>
    if(bestKnown < Int.MaxValue && bestKnown > Int.MinValue) xml += <best_known_objective>{bestKnown}</best_known_objective>

    xml += (<solutions>{solutions.map(_.asXml)}</solutions>)

    <results>{xml}</results>
  }

  /**
    * writes the specified elements into an xml file.
    * @param directory the path of a directory where the file will be created
    * @param config the name of the config which is the first part of the filename
    * @param instance the name of the instance which is the second part of the filename
    * @param bestKnown the objective value of the best known solution if known.
    * @param maxObjective whether the objective is to maximise (true) or minimise (false).
    * @param solutions an iterable of pairs Time (Long) - solution objective (Int).
    * @return an xml node.
    */
  def writeToXml(
                  directory: String,
                  config: String,
                  timeout: Long,
                  instance: String,
                  problem: String = "unknown",
                  bestKnown: Int = Int.MaxValue,
                  maxObjective: Boolean,
                  solutions: Iterable[CPIntSol]
                ): Unit = {
    val xml = resultsToXml(config, timeout, instance, problem, bestKnown, maxObjective, solutions)
    val output = directory + "/" + problem + "/" + config + "_" + instance + ".xml"
    IOUtils.saveToFile(output, new PrettyPrinter(80, 4).format(xml))
  }
}