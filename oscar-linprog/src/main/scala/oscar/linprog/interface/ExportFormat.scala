package oscar.linprog.interface

sealed abstract class ExportFormat(val extension: String)

case object LP extends ExportFormat("lp")
case object MPS extends ExportFormat("mps")
