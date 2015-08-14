package oscar.lcg.core

abstract class Constraint {

  def store: LCGStore
  
  def name: String
  
  def setup(): LiftedBoolean 
  
  def propagate(): Boolean
  
  def explain(): Unit
}