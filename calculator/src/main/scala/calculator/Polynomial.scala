package calculator

import scala.math.sqrt


object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal {
      val ba = b()
      val aa = a()
      val ca = c()
      ba * ba - 4 * aa * ca
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val deltaVal = delta()
      val ba = b()
      val aa = a()
      if (deltaVal < 0) Set() // No real solutions
      else if (deltaVal == 0) Set(-ba / (2 * aa)) // One real solution
      else { // Two real solutions
        val sqrtDelta = sqrt(deltaVal)
        Set(
          (-ba + sqrtDelta) / (2 * aa),
          (-ba - sqrtDelta) / (2 * aa)
        )
      }
    }
