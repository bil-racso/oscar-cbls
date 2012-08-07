package oscar.cp.scheduling

import java.security.InvalidParameterException

// Precedence Types
private class PrecedenceType
private object EBE extends PrecedenceType { override def toString = "End before End"}
private object EBS extends PrecedenceType { override def toString = "End before Start"}
private object SBE extends PrecedenceType { override def toString = "Start before End"}
private object SBS extends PrecedenceType { override def toString = "Start before Start"}
private object EAE extends PrecedenceType { override def toString = "End at End"}
private object EAS extends PrecedenceType { override def toString = "End at Start"}
private object SAE extends PrecedenceType { override def toString = "Start at End"}
private object SAS extends PrecedenceType { override def toString = "Start at Start"}

class ActivityPrecedence(act1 : Activity, act2 : Activity, pType : PrecedenceType) {
	
	def withDelay(delay : Int) = {
		
		pType match {
			case EBE => (act1.end   + delay <= act2.end)
			case EBS => (act1.end   + delay <= act2.start)
			case SBE => (act1.start + delay <= act2.end)
			case SBS => (act1.start + delay <= act2.start)
			case EAE => (act1.end   + delay == act2.end)
			case EAS => (act1.end   + delay == act2.start)
			case SAE => (act1.start + delay == act2.end)
			case SAS => (act1.start + delay == act2.start)
			case _   => throw new InvalidParameterException(pType + " is not a valid precedence type.")
		}
	}
}

object ActivityPrecedence {
	
	// Before
	def endBeforeEnd(act1 : Activity, act2 : Activity) 	   = new ActivityPrecedence(act1, act2, EBE)
	def endBeforeStart(act1 : Activity, act2 : Activity)   = new ActivityPrecedence(act1, act2, EBS)
	def startBeforeEnd(act1 : Activity, act2 : Activity)   = new ActivityPrecedence(act1, act2, SBE)
	def startBeforeStart(act1 : Activity, act2 : Activity) = new ActivityPrecedence(act1, act2, SBS)
	
	// At
	def endAtEnd(act1 : Activity, act2 : Activity)     = new ActivityPrecedence(act1, act2, EAE)
	def endAtStart(act1 : Activity, act2 : Activity)   = new ActivityPrecedence(act1, act2, EAS)
	def startAtEnd(act1 : Activity, act2 : Activity)   = new ActivityPrecedence(act1, act2, SAE)
	def startAtStart(act1 : Activity, act2 : Activity) = new ActivityPrecedence(act1, act2, SAS)
}
