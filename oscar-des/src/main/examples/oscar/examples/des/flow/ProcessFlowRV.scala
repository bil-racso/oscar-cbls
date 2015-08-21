package oscar.des.flow

import JSci.maths.statistics.UniformDistribution
import oscar.des.engine.Model
import oscar.des.flow.lib._
import oscar.des.montecarlo.{DoubleRandomVar, IntRandomVar}

/*
object TestBeltRV extends App with HelperForProcess{

  val m = new Model
  val verbose = false

  //a process that has two inputs, and one output (eg: soldering)
  println("start simulate...")

  val inputStRV = new IntRandomVar("inputStRV", new UniformDistribution(0, 200))
  val outputStRV = new IntRandomVar("outputStRV", new UniformDistribution(0, 25))
  val slowDurationRV = new IntRandomVar("slowDurationRV", new UniformDistribution(0, 15))

  val input = new Storage(inputStRV.staticIntRandomFunc, 10, "input", verbose)
  val output = new Storage(outputStRV.staticIntRandomFunc, 0, "output", verbose)

  val belt = new ConveyorBeltProcess(m, 5, 1, List((1, input)), List((1, output)), "belt", verbose)
  val slowlyFeedingInput  = new SingleBatchProcess(m, slowDurationRV.staticFloatRandomFunc, List(), List((13, input)), "slowlyFeedingInput", verbose)

  m.simulate(100, verbose)

  println("done.")
  println()
  println(input)
  println(output)
  println(belt)
  println(slowlyFeedingInput)
}

object TestFailingProcessRV extends App with HelperForProcess{
  val m = new Model
  val verbose = true

  //a process that has two inputs, and one output (eg: soldering)
  println("start simulate...")

  val inputStRV = new IntRandomVar("inputStRV", new UniformDistribution(0, 200))
  val inputStContentRV = new IntRandomVar("inputStContentRV", new UniformDistribution(0, 10))
  val outputStRV = new IntRandomVar("outputStRV", new UniformDistribution(0, 25))
  val trashOSRV = new IntRandomVar("trashOSRV", new UniformDistribution(1, 8))
  val failureRV = new IntRandomVar("failureRV", new UniformDistribution(0, 3))
  val successRV = new DoubleRandomVar("successRV", new UniformDistribution(0, 3))

  val input = new Storage(inputStRV.staticIntRandomFunc, inputStContentRV.staticIntRandomFunc, "input", verbose)
  val output = new Storage(outputStRV.staticIntRandomFunc, 0, "output", verbose)
  val trash = new OverflowStorage(trashOSRV.staticIntRandomFunc, 0, "trash", verbose)
  val process = FailingSingleBatchProcess(m, 5,  List((1, input)), List((1, output)), List((failureRV.staticIntRandomFunc, trash)),
                                          () => successRV.getValue > 1, "failing process", verbose)

  m.simulate(100, verbose)

  println("done.")
  println()
  println(input)
  println(output)
  println(trash)
  println(process)
}

object TestProcessRV extends App with HelperForProcess{

  val m = new Model
  val verbose = true

  //a process that has two inputs, and one output (eg: soldering)
  println("start simulate...")

  val stockAInitCRV = new IntRandomVar("stockAInitCRV", new UniformDistribution(0, 200))
  val stockA = new Storage(200, stockAInitCRV.staticIntRandomFunc, "stockA", verbose)

  val stockBInitCRV = new IntRandomVar("stockBInitCRV", new UniformDistribution(200, 300))
  val stockB = new Storage(300, stockBInitCRV.staticIntRandomFunc, "stockB", verbose)

  val supplyADelayRV = new IntRandomVar("supplyADelayRV", new UniformDistribution(50, 100))
  val supplyAPercentRV = new IntRandomVar("supplyAPercentRV", new UniformDistribution(0, 100))
  val supplierforA = new PartSupplier(m, supplyADelayRV.staticIntRandomFunc, supplyAPercentRV.staticIntRandomFunc,"SupplierForA", verbose)

  //  OrderOnStockTreshold(stockA, 30, _ => 100, supplierforA, verbose)
  val orderPeriodRV = new IntRandomVar("orderPeriodRV", new UniformDistribution(50, 100))
  val orderPolicy = new OrderOnStockThresholdWithTick(stockA, m, 30, orderPeriodRV.staticFloatRandomFunc, _ => 100, supplierforA, verbose, "order")

  val middleStock = new Storage(7, 0, "middleStock", verbose)
  val trash = new OverflowStorage(5, 0, "trash", verbose)

  val emptyDurationRV = new IntRandomVar("emptyDurationRV", new UniformDistribution(25, 30))
  val emptyingTrash = SingleBatchProcess(m, emptyDurationRV.staticFloatRandomFunc, List((2, trash)), List(), "emptyingTrash", verbose)

  val solderingDurationRV = new IntRandomVar("solderingDurationRV", new UniformDistribution(10, 15))
  val soldering = BatchProcess(m, 2, 13, List((1, stockA),(2, stockB)), List((1, middleStock),(1, trash)), "soldering", verbose)

  val outputStock = new Storage(50, 10, "outputStock", verbose)

  val attachingDurationRV = new IntRandomVar("attachingDurationRV", new UniformDistribution(10, 15))
  val attaching = SingleBatchProcess(m, attachingDurationRV.staticFloatRandomFunc, List((3, middleStock),(1, stockA)), List((1, outputStock)), "attaching", verbose)

  val deliveringDurationRV = new IntRandomVar("deliveringDurationRV", new UniformDistribution(45, 70))
  val delivering = SingleBatchProcess(m, deliveringDurationRV.staticFloatRandomFunc, List((20, outputStock)), List(), "delivering", verbose)

  m.simulate(10000, verbose)

  println("done.")
  println()
  println(stockA)
  println(stockB)
  println(outputStock)
  println(trash)
  println(soldering)
  println(attaching)
  println(delivering)
  println(orderPolicy)
  println(supplierforA)
}

object SimpleFactoryRV extends App with HelperForProcess{

  val m = new Model
  val verbose = true

  //a process that has two inputs, and one output (eg: soldering)
  println("start simulate...")

  // Initial content os stocks is random
  val stockAContentRV = new IntRandomVar("stockAContentRV", new UniformDistribution(0, 100))
  val stockA = new Storage(100, stockAContentRV.staticIntRandomFunc, "stockA", verbose)
  val stockBContentRV = new IntRandomVar("stockBContentRV", new UniformDistribution(0, 300))
  val stockB = new Storage(300, stockBContentRV.staticIntRandomFunc, "stockB", verbose)

  val middleStock = new Storage(7, 0, "middleStock", verbose)

  val trash = new OverflowStorage(5, 0, "trash", verbose)

  // Random duration
  val emptyingDurationRV = new IntRandomVar("emptyingDurationRV", new UniformDistribution(20, 30))
  val emptyingTrash = SingleBatchProcess(m, emptyingDurationRV.staticFloatRandomFunc, List((3, trash)), List(), "emptyingTrash", verbose)

  val solderingDurationRV = new IntRandomVar("solderingDurationRV", new UniformDistribution(8, 16))
  val soldering = BatchProcess(m, 2, solderingDurationRV.staticFloatRandomFunc, List((1, stockA),(2, stockB)), List((1, middleStock),(1, trash)), "soldering", verbose)

  val outputStock = new Storage(50, 0, "outputStock", verbose)

  val attachingDurationRV = new IntRandomVar("attachingDurationRV", new UniformDistribution(10, 20))
  val attaching = SingleBatchProcess(m, attachingDurationRV.staticFloatRandomFunc, List((3, middleStock),(1, stockA)), List((1, outputStock)), "attaching", verbose)

  val deliveringDurationRV = new IntRandomVar("deliveringDurationRV", new UniformDistribution(6, 15))
  val delivering = SingleBatchProcess(m, deliveringDurationRV.staticFloatRandomFunc, List((20, outputStock)), List(), "delivering", verbose)

  m.simulate(10000, verbose)

  println("done.")
  println()
  println(stockA)
  println(stockB)
  println(outputStock)
  println(trash)
  println(soldering)
  println(attaching)
  println(delivering)
}
*/