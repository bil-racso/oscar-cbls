/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.algebra


/**
 * Represents a continuous interval.
 * 
 * @param lowerBound is the lower bound of the interval
 * @param lbInclusive true if the lower bound belongs to the interval
 * @param upperBound is the upper bound of the interval
 * @param ubInclusive true if the upper bound belongs to the interval
 *
 * @author acrucifix acr@n-side.com
 */
case class Interval(lowerBound: Double, lbInclusive: Boolean, upperBound: Double, ubInclusive: Boolean) {
  if(lbInclusive && ubInclusive) require(lowerBound <= upperBound, "The lower bound should be smaller or equal to the upper bound")
  else require(lowerBound < upperBound, "The lower bound should be strictly smaller than the upper bound.")

  /**
   * Returns true if this contains the given value
   */
  def contains(x: Double): Boolean =
    (lbInclusive, ubInclusive) match {
      case (true , true)  => lowerBound <= x && x <= upperBound
      case (false, true)  => lowerBound < x  && x <= upperBound
      case (true , false) => lowerBound <= x && x < upperBound
      case (false, false) => lowerBound < x  && x < upperBound
    }

  /**
   * Returns if this and that intersects
   */
  def intersect(that: Interval): Boolean =
    (
      if(this.lbInclusive && that.ubInclusive) this.lowerBound <= that.upperBound && (
          if(this.ubInclusive && that.lbInclusive) this.upperBound >= that.lowerBound
          else  this.upperBound > that.lowerBound
        )
      else this.lowerBound < that.upperBound && (
          if(this.ubInclusive && that.lbInclusive) this.upperBound >= that.lowerBound
          else  this.upperBound > that.lowerBound
        )
    ) || (
      if(this.ubInclusive && that.lbInclusive) this.upperBound >= that.lowerBound && (
          if(this.lbInclusive && that.ubInclusive) this.lowerBound <= that.lowerBound
          else this.lowerBound < that.lowerBound
        )
      else this.upperBound > that.lowerBound && (
          if(this.lbInclusive && that.ubInclusive) this.lowerBound <= that.lowerBound
          else this.lowerBound < that.lowerBound
        )
    )

  override def toString = (if(lbInclusive) "[" else "]") + s"$lowerBound; $upperBound" + (if(ubInclusive) "[" else "]")
}

object Singleton {
  def apply(d: Double) = new Interval(d, lbInclusive = true, d, ubInclusive = true)
}

/**
 * Represents a linear function defined on a certain interval: f(x) defined on g(x) in interval.
 *
 * @param ordinate = f(x), the [[LinearExpression]] giving the value of this [[LinearPiece]].
 * @param abscissa = g(x), the [[LinearExpression]] whose value should be in the interval for the [[LinearPiece]] to be defined.
 * @param interval the interval to which the value of the abscissa should belong for the [[LinearPiece]] to be defined.
 *
 * @author acrucifix acr@n-side.com
 */
class LinearPiece(val ordinate: LinearExpression, val abscissa: LinearExpression, val interval: Interval) extends Expression {
  def uses[V <: Var](v: V) = ordinate.uses(v) || abscissa.uses(v)

  override def eval(env: Var => Double): Double =
    if(interval.contains(abscissa.eval(env))) ordinate.eval(env)
    else throw new IllegalArgumentException("This LinearPiece is not defined at the given point.")

  override def value: Option[Double] = abscissa.value.flatMap { x =>
    if(interval.contains(x)) ordinate.value
    else None
  }

  override def derive(x: Var): Expression =
    new LinearPiece(
      ordinate.derive(x).asInstanceOf[LinearExpression],
      abscissa,
      interval
    )

  def *(c: Const): LinearPiece = new LinearPiece(c * ordinate, abscissa, interval)

  def unary_- : LinearPiece = new LinearPiece(-ordinate, abscissa, interval)

  def +(expr: LinearExpression): LinearPiece = new LinearPiece(ordinate + expr, abscissa, interval)
  def -(expr: LinearExpression): LinearPiece = new LinearPiece(ordinate - expr, abscissa, interval)

  override def toString = s"$ordinate | $abscissa in $interval"
}

/**
 * Represents a piecewise linear expression.
 *
 * A piecewise linear function is the sum of several [[LinearPiece]].
 *
 * Examples:
 *  f(x) = {
 *    2     on -Inf < x < 0;
 *    x - 4 on   0 <= x < 4;
 *    -x    on   4 <= x < +Inf
 *    }
 *
 *  f(2 x + 1) = {
 *    2 x + 1           on -Inf < 2 x + 1 < 0;
 *    0                 on   0 <= 2 x + 1 < 10;
 *    0.5 (2 x + 1) - 2 on  10 <= 2 x + 1 < 20;
 *    -2 (2 x + 1)      on  20 <= 2 x + 1 < Inf
 *  }
 *
 * @author acrucifix acr@n-side.com
 */
class PiecewiseLinearExpression(val pieces: Seq[LinearPiece]) extends Expression {
  require(pieces.size > 0, "A PiecewiseLinearExpression should have at least one LinearPiece")

  def uses[V <: Var](v: V) = pieces.exists(p => p.uses(v))

  def eval(env: Var => Double): Double =
    pieces.filter { piece =>
      piece.interval.contains(piece.abscissa.eval(env))
    }.map { piece =>
      piece.ordinate.eval(env)
    }.sum

  def value: Option[Double] =
    pieces.filter { piece =>
      piece.abscissa.value match {
        case Some(x) => piece.interval.contains(x)
        case None => true
      }
    }.map { piece =>
      piece.ordinate.value
    }.reduce[Option[Double]] {
      case (Some(d1), Some(d2)) => Some(d1 + d2)
      case (Some(d1), None)     => Some(d1)
      case (None    , Some(d2)) => Some(d2)
      case (None    , None)     => None
    }

  def derive(x: Var): Expression = pieces.map{_.derive(x)}.reduce(_ + _)

  def *(c: Const): PiecewiseLinearExpression = new PiecewiseLinearExpression(pieces.map(p => p * c))

  def unary_- : PiecewiseLinearExpression = new PiecewiseLinearExpression(pieces.map(p => -p))

  def +(piece: LinearPiece) = new PiecewiseLinearExpression(piece +: pieces)
  def +(pcwle: PiecewiseLinearExpression) = new PiecewiseLinearExpression(this.pieces ++ pcwle.pieces)
  def +(linExpr: LinearExpression) = new PiecewiseLinearExpression(linExpr.toLinearPiece() +: pieces)

  def -(piece: LinearPiece) = new PiecewiseLinearExpression((-piece) +: pieces)
  def -(pcwle: PiecewiseLinearExpression) = new PiecewiseLinearExpression(this.pieces ++ (-pcwle).pieces)
  def -(linExpr: LinearExpression) = new PiecewiseLinearExpression((-linExpr).toLinearPiece() +: pieces)

  override def toString = s"{${pieces.map(_.toString).mkString(", ")}}"
}
