/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.cp.minizinc

import java.io.FileReader

class Options(args: Array[String]) {

  var file: FileReader = null

  var all = false

  var verbose = false

  var statistics = false

  var timeOut = 0

  var nSolutions = -1
  //println("<==>")
  if (args.length == 0) {
    System.out.println("fz2oscar: no model file specified");
    System.out.println("fz2oscar: use --help for more information.");
    System.exit(0);
  } else if (args.length == 1) {
    val arg = args(0)
    if (arg.equals("-h") || arg.equals("--help")) {
      println(
        "Usage: java oscar.minizinc.Fz2OscaR [<options>] <file>.fzn\n" +
          "Options:\n" +
          "    -h, --help\n" +
          "        Print this message.\n" +
          "    -a, --all, --all-solutions\n" +
          "    -v, --verbose\n" +
          "    -t <value>, --time-out <value>\n" +
          "        <value> - time in second.\n" +
          "    -s, --statistics\n" +
          "    -n <value>, --num-solutions <value>\n" +
          "        <value> - limit on solution number.\n")
      System.exit(0);
    } else { // input file
      file = new FileReader(args(0))
    }
  } else { // args.length > 1
    var i = 0
    def oneOf(poss: String*) = poss.exists(args(i) == _)
    while (i < args.length - 1) {
      if (oneOf("-a", "--all-solutions", "--all")) {
        all = true
        i += 1
      } else if (oneOf("-t", "--time-out")) {
        timeOut = args(i + 1).toInt
        i += 2
      } else if (oneOf("-s", "--statistics")) {
        statistics = true
        i += 1
      } else if (oneOf("-n", "--num-solutions")) {
        nSolutions = args(i + 1).toInt
        i += 2
      } else if (oneOf("-v", "--verbose")) {
        verbose = true;
        i += 1
      } else {
        println("fz2oscar: not recognized option " + args(i))
        i += 1
      }
    }
    try {
      file = new FileReader(args.last);
    } catch {
      case e: java.io.FileNotFoundException => {
        println("Flatzinc2OscaR Parser Version XX:  File " + args.last + " not found.")
        System.exit(0);
      }
    }
  }

}