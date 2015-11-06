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
 * Represents the sign of a [[LinearExpression]]
 *
 * sign(f(x)) =
 *   {
 *      -1  for f(x) < 0;
 *       0  for f(x) = 0;
 *       1  for f(x) > 0
 *   }
 *
 * @author acrucifix acr@n-side.com
 */
case class Sign(linExpr: LinearExpression, lowerBound: Double = -Double.MaxValue, upperBound: Double = Double.MaxValue)
  extends PiecewiseLinearExpression(
    pieces = {
      require(upperBound > lowerBound, s"The upper bound $upperBound should be greater than the lower bound $lowerBound.")

      val minus =
        if(lowerBound < 0) Some(new LinearPiece(-1, linExpr, Interval(lowerBound, lbInclusive = true, 0, ubInclusive = true)))
        else if(lowerBound == 0) Some(new LinearPiece(0, linExpr, Interval(lowerBound, lbInclusive = true, 0, ubInclusive = true)))
        else None

      val plus =
        if(upperBound > 0) Some(new LinearPiece(1, linExpr, Interval(0, lbInclusive = true, upperBound, ubInclusive = true)))
        else if(upperBound == 0) Some(new LinearPiece(0, linExpr, Interval(0, lbInclusive = true, upperBound, ubInclusive = true)))
        else None

      Seq(minus, plus).flatten
    }
  ) {

  override def toString = s"sign($linExpr)"
}
