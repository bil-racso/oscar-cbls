package scampi.cp.examples

import scampi.cp.modeling._
import scampi.search._
import collection.immutable.SortedSet

/*
 * Problem statement :
 * There are 13 words : BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, JUNK, LIMN, QUIP, SWAG, VISA, WISH
 * 24 different letters that appear in the 13 words.
 * Assign the 24 letters appearing to 4 different cubes (one letter/face) so that the 4 letters of each word appears on different cubes.
 */

object Cubes extends CPModel {

  def main(args: Array[String]): Unit =
  {
    val cp = CPSolver()
    
    val numCubes = 4
    val numFaces = 6
    
    val words = "BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, JUNK, LIMN, QUIP, SWAG, VISA, WISH".split(", ")
    val letters = words.foldLeft(SortedSet.empty[Char]){_++_}.toSeq // Set of all 24 letters
    val numLetters = letters.size
    def letterToInt(letter: Char): Int = letters.indexOf(letter) // Letter from letter index
    
    val placement = for(i <- 0 until numLetters) yield CPVarInt(cp, 0 until numCubes) // The cube (0 to 3) on which each letter is placed
    
    cp.solve() subjectTo
    {
      cp.add(gcc(placement, 0 until numCubes, numFaces, numFaces), Strong) // There must be exactly 6 letters on each cube
      for(word <- words)
        cp.add(alldifferent( // The 4 letters of each word must be placed on different cubes
            for(letter <- word.toCharArray()) yield placement(letterToInt(letter))
        ), Strong)
    } exploration { // Each letter will be assigned different cubes during the search
      loop(0 until numLetters) {
        l =>  cp.branchAll(0 until numCubes)(v => cp.post(placement(l) == v))
      }
      
      for (cube <- 0 until numCubes) { // Printing the letters placed on each cube
        println("Cube "+cube+" : "+
            placement.zipWithIndex.map {
              case (v,i) => if (v.getValue()==cube) letters(i) else "."}.mkString(" "))
      }
    }
    
    cp.printStats()
  }
}