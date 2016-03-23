package oscar.cbls.invariants.core.algo.seq

import oscar.cbls.invariants.core.algo.quick.QList

/**
 * this is about linked knuth list
 * that can maintain a link between two lists to ease the propagation of updates one one list to the other one.
 * besides,it also adds a lazy position counter that is updated only on query (hope there will be no such query, actually)
 */
