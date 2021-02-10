package questions

object GoodEnough extends App{

  def abs(x: Double ) =
    if (x<0) - x
    else x

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x)/x < 0.01

  def sqrIter(guess: Double, x: Double ): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrIter(improve(guess,x),x)

  def improve(guess: Double, x:Double) =
    (guess+x / guess)/2

  def sqrt(x:Double) = sqrIter(1.0,x)


  println(sqrt(2))
  println(sqrt(6))
  println(sqrt(1e-7))
  println(sqrt(1e50))
}

