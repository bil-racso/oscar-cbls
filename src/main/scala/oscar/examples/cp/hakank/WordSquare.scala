/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.util.matching._

/*

  WordSquare in Oscar.

  From http://en.wikipedia.org/wiki/Word_square
  """
  A word square is a special case of acrostic. It consists of a set of words,
  all having the same number of letters as the total number of words (the
  'order' of the square); when the words are written out in a square grid
  horizontally, the same set of words can be read vertically.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object WordSquare extends CPModel {


 /*
   *
   * Read the words from a word list with a specific word length.
   *
   */
  def readWords(word_list: String, word_len: Int) : Array[String] = {
    
    println("reading from " + word_list + " (size: " + word_len + ")");
    val words = scala.io.Source.fromFile(word_list, "utf-8").getLines

    // There just must be a simpler way of just matching a regexp
    // than using findAllIn and then check for the length of matching
    // entries...
    val Rex = new Regex("^([a-zA-Z]+)$") // exclude all "weird" words
    var all_words = List[String]()
    val seen = scala.collection.mutable.HashMap.empty[String, Boolean].withDefaultValue(false)
    for (w <- words) {    
      val w2 : String = w.trim().toLowerCase()
      if(
         w2.length > 0         && 
         w2.length == word_len &&
         Rex.findAllIn(w2).toList.length > 0 &&
         !seen(w2)
         ) {
         all_words ::= w2
         seen += (w2 -> true)
      }
    }
    
    return all_words.reverse.toArray

  }



  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var word_list = "/usr/share/dict/words"
    var word_len = 5
    println("word_len:"+word_len)
    var num_to_show = 20

    if (args.length > 0) {
      word_list = args(0)
    }

    if (args.length > 1) {
      word_len = args(1).toInt
    }

    if (args.length > 2) {
      num_to_show = args(2).toInt
    }

    val WORDLEN = 0 until word_len

    // Convert letters <=> digits
    val alpha = "abcdefghijklmnopqrstuvwxyz"
    val d = scala.collection.mutable.HashMap.empty[Char,Int]
    var count = 1
    for(i <- 0 until alpha.length) {
      d += (alpha(i) -> count)
      count += 1
    }

    val num_letters = alpha.length;
   
    // Read the word list
    val words = readWords(word_list, word_len)
    var num_words = words.length // set after reading the wordlist
    println("number of words: " + num_words)

    //
    // variables
    //
    // word matrix 
    val A = Array.tabulate(num_words,word_len)((i,j) => d(words(i)(j)))
    val A_flat = A.flatten

    // the selected words
    val E = Array.fill(word_len)(CPVarInt(cp, 0 to num_words))


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

        cp.add(alldifferent(E),Weak)


        // now find the connections
        for(i <- WORDLEN) {
          for(j <- WORDLEN) {
            cp.add(element(A, E(i),CPVarInt(cp,j)) == element(A, E(j),CPVarInt(cp,i)))
          }
        }


    } exploration {

      cp.binaryFirstFail(E)

      println("solution #" + (numSols+1))
      E.foreach(e=> println(words(e.getValue())))
      println()

      numSols += 1
      if (num_to_show > 0 && numSols >= num_to_show) {
        cp.stop();
      }


   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
