package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._
import oscar.cp.scheduling._

class TestSweepCumulative extends FunSuite with ShouldMatchers {

  test("Test 1: MinSweepCumulative") {

    val horizon = 8
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 2 to 4)
    val act2 = Activity(cp, 0 to 2)

    val cumAct1 = CumulativeActivity(act1, 0, -1 to 1)
    val cumAct2 = CumulativeActivity(act2, 0 to 1, -3 to 4)

    cp.add(act1.start >= 1)
    cp.add(act1.start <= 2)

    cp.add(act2.start >= 0)
    cp.add(act2.start <= 6)

    val acts = Array(cumAct1, cumAct2)

    val constraint1 = new MinSweepCumulative(cp, acts, 4, 0)
    val constraint2 = new MinSweepCumulative(cp, acts, 3, 1)

    cp.add(constraint1)
    cp.add(constraint2)

    cumAct2.est should be(1)
    cumAct2.lst should be(2)
    cumAct2.ect should be(3)
    cumAct2.lct should be(4)
    cumAct2.minDuration should be(1)
    cumAct2.maxDuration should be(2)
    cumAct2.minHeight should be(3)
    cumAct2.maxHeight should be(4)
  }

  test("Test 2: alternatives") {

    val horizon = 6
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 4)
    val act3 = CumulativeActivity(Activity(cp, 6), 0 to 1, 2)
    val act4 = CumulativeActivity(Activity(cp, 6), 0 to 1, 3)
    val acts = Array(act1, act2, act3, act4)

    var nbSol = 0
    val expectedSol = Set((0, 0, 1, 1), (1, 1, 0, 0))

    cp.solve subjectTo {

      cp.add(new MaxSweepCumulative(cp, acts, 5, 0))
      cp.add(new MaxSweepCumulative(cp, acts, 5, 1))
      cp.add(new MinSweepCumulative(cp, acts, 5, 0))
      cp.add(new MinSweepCumulative(cp, acts, 5, 1))

    } exploration {
      cp.binary(acts.map(_.resource))
      val sol = (acts(0).resource.value, acts(1).resource.value, acts(2).resource.value, acts(3).resource.value)
      expectedSol.contains(sol) should be(true)
      nbSol += 1
    } run()
      nbSol should be(2)
  }

  test("Test 3: alternatives") {

    val horizon = 6
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 4)
    val act3 = CumulativeActivity(Activity(cp, 6), 0 to 1, 2)
    val act4 = CumulativeActivity(Activity(cp, 6), 0 to 1, 3)
    val acts = Array(act1, act2, act3, act4)

    var nbSol = 0
    val expectedSol = Set((0, 0, 1, 1), (1, 1, 0, 0))

    cp.solve subjectTo {

      cp.add(new MaxSweepCumulative(cp, acts, 5, 0))
      cp.add(new MaxSweepCumulative(cp, acts, 5, 1))
      cp.add(acts(1).resource == 0)

      acts(0).resource.value should be(0)
      acts(2).resource.value should be(1)
      acts(3).resource.value should be(1)

    } exploration {
      cp.binary(acts.map(_.resource))
      nbSol += 1
    } run()
    nbSol should be(1)
  }

  test("Test 4: MaxSweepCumulative") {

    val horizon = 106
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 4)
    val act3 = CumulativeActivity(Activity(cp, 6), 0 to 1, 2)
    val act4 = CumulativeActivity(Activity(cp, 6), 0 to 1, 3)
    val act5 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val acts = Array(act1, act2, act3, act4, act5)

    cp.add(act1.start == 0)
    cp.add(act2.start == 0)
    cp.add(act3.start == 0)
    cp.add(act4.start == 0)

    cp.add(new MaxSweepCumulative(cp, acts, 5, 0))
    cp.add(new MaxSweepCumulative(cp, acts, 5, 1))
    cp.add(acts(1).resource == 0)

    acts(0).resource.value should be(0)
    acts(2).resource.value should be(1)
    acts(3).resource.value should be(1)
    acts(4).resource.size should be(2)
    cp.add(acts(4).resource == 0)
    acts(4).start.min should be(6)

  }

  test("Test 5: MaxSweepCumulative") {

    val horizon = 6
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 0 to 5)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, -2 to 5)

    cp.add(new MaxSweepCumulative(cp, Array(act1, act2), 3, 0))

    cp.add(act1.resource == 0)
    cp.add(act2.resource == 0)
    cp.add(act1.height == 5) // it means act2 must have a negative consumption of -2

    act2.height.value should be(-2)

  }

  test("Test 6: MaxSweepCumulative") {

    val horizon = 9
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 0 to 5), 0 to 1, 0 to 5)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 0 to 5)

    cp.add(new MaxSweepCumulative(cp, Array(act1, act2), 3, 0))

    cp.add(act1.resource == 0)
    cp.add(act1.height == 2)
    cp.add(act2.height == 2)
    cp.add(act2.resource == 0)
    cp.add(act2.start == 3)

    act1.dur.min should be(0)
    act1.dur.max should be(3)

  }
}
