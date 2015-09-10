package oscar.cp.searches

import oscar.cp.testUtils._
import oscar.algo.search.SearchNode
import oscar.algo.search.noAlternative
import oscar.algo.search.Alternative
import oscar.algo.search.branchAll
import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching

class DiscrepancyBranchingSuite extends TestSuite {

  test("test discrepancy 1") {
    val node = new SearchNode()
    val i = new ReversibleInt(node, 0)
    val branching = new Branching {
      override def alternatives(): Seq[Alternative] = {
        if (i > 0) noAlternative
        else branchAll(1 to 3) { v => i += 1 }
      }
    }
    node.search { new DiscrepancyBranching(node, branching, 1) }
    val stat = node.start()
    assert(stat.nSols == 2)
  }

  test("test discrepancy 2") {
    val node = new SearchNode()
    val i = new ReversibleInt(node, 0)
    val branching = new Branching {
      override def alternatives(): Seq[Alternative] = {
        if (i > 2) noAlternative
        else branchAll(1 to 3) { v => i += 1 }
      }
    }
    node.search { new DiscrepancyBranching(node, branching, 5) }
    val stat = node.start()
    assert(stat.nSols == 26)
  }

}