package oscar.des.flow.lib

/*class Variable[T]
class Proposition extends Variable[Boolean]
class Entier extends Variable[Int]
class IntConstant() extends Variable[Int]
class TruthValue() extends Variable[Boolean]

//basic properties on simulation elements
class Empty(s:Storage) extends Proposition
class NonProductive(p:Process) extends Proposition
case class Productivity(p:Process) extends Entier

//logical properties
//we only consider temporal operators of the past, easy to evaluate
class Not(f:Proposition) extends Proposition
class And(f:Proposition, g:Proposition) extends Proposition
class Or(f:Proposition, g:Proposition) extends Proposition

class HasAlwaysBeen(f:Proposition) extends Proposition
class HasBeen(f:Proposition) extends Proposition
class Since(a:Proposition,b:Proposition) extends Proposition  //the counterpart is only differing with its initial value

//selecting some events (thus value transitions on fluents)
//events happen at event time; besides, they have no interpretation.
class Event
class BT(p:Proposition) extends Proposition
class BF(p:Proposition) extends Proposition
class Changes(p:Proposition) extends Proposition

//variables always have a value.
class Duration(start:Proposition, end:Proposition) extends Entier
class Sum(s:Variable) extends Entier
class Mult(a:Variable,b:Variable) extends Entier
class Plus(a:Variable,b:Variable) extends Entier
class PonderateWithDuration(s:Variable) extends Entier

//relational operators to get back to Propositions
class G(a:Entier,b:Entier) extends Proposition
class GE(a:Entier,b:Entier) extends Proposition
class LE(a:Entier,b:Entier) extends Proposition
class EQ(a:Entier,b:Entier) extends Proposition
class NEQ(a:Entier,b:Entier) extends Proposition

//To estimate over different runs
//how to find names that are obviously statistics over different runs
class Statistics
class Average(s:Stream) extends Statistics

class Model(){
  val durationSinceLastTick:Entier
  val currentTime:Entier
}
*/