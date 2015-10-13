package oscar.linprog.test

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import oscar.linprog.enums.ModelExportFormat
import oscar.linprog.modeling._

@RunWith(classOf[JUnitRunner])
class ModelExportTester extends FunSuite with Matchers with BeforeAndAfter {

  def exportPath(format: ModelExportFormat) = Paths.get(s"test.${format.extension}")

  after {
    for (format <- ModelExportFormat.formats) {
      exportPath(format).toFile.delete()
    }
  }

  test("Export model before solve") {
    for {
      format <- ModelExportFormat.formats
      _solver <- MPSolver.lpSolvers
    } {
      implicit val solver = _solver

      val x = MPFloatVar("x", 100, 150)
      val y = MPFloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <= 200)

      solver.exportModel(exportPath(format), format)
    }
  }

  test("Export model after solve") {
    for {
      format <- ModelExportFormat.formats
      _solver <- MPSolver.lpSolvers
    } {
      implicit val solver = _solver

      val x = MPFloatVar("x", 100, 150)
      val y = MPFloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <= 200)

      solver.solve

      solver.exportModel(exportPath(format), format)
    }
  }
}
