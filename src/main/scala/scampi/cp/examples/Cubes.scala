package scampi.cp.examples

import scampi.cp.modeling._
import scampi.search._

object Cubes extends CPModel {

  def main(args: Array[String]): Unit =
  {
    val cp = CPSolver()
    
    val numCubes = 4
    val numFaces = 6
    val numLetters = 24
    
    val words = "BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, JUNK, LIMN, QUIP, SWAG, VISA, WISH".split(", ")
    val intToLetter = for(i <- "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,Y".split(",")) yield i.charAt(0)
    def letterToInt(letter: Char): Int = intToLetter.indexOf(letter)
    
    var placement = for(i <- 0 until numLetters) yield CPVarInt(cp, 0 until numCubes)
    
    var nbSol = 0
    
    cp.solve subjectTo
    {
      cp.add(gcc(placement, 0 until numCubes, numFaces, numFaces), Strong)
      for(word <- words) cp.add(alldifferent(for(letter <- word.toCharArray()) yield placement(letterToInt(letter))), Strong)
    } exploration {
      loop(0 until numLetters) {
        l =>  cp.branchAll(0 until numCubes)(v => cp.post(placement(l) == v))
      }
      nbSol += 1
      
      for (cube <- 0 until numCubes) {
        println("Cube "+cube+" : "+(for(i <- 0 until numLetters) yield {
          if(placement(i).getValue() == cube) intToLetter(i)
          else "."
          }).mkString(" ")
        )
      }
    }
    
    println("#sol:"+nbSol)
    cp.printStats()
  }
}