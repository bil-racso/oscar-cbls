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
	
	def apply(act1 : Activity, act2 : Activity, pType : PrecedenceType) = new ActivityPrecedence(act1, act2, pType)
}

