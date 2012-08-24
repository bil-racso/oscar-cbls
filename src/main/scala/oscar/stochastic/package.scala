package oscar

import JSci.maths.statistics.ProbabilityDistribution
package object stochastic {

	implicit def valueToDistr[A](value: A) = new ValueDistr(value)
	implicit def probaDistr2Distr(d: ProbabilityDistribution) = new Distribution(d)
}