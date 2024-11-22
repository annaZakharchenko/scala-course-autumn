package mexmath.foundation.week1.topic

import scala.annotation.targetName

object arithmetic:

  sealed trait Expression:
    def evaluate: Nat3

  sealed trait Nat3 extends Expression:
    val evaluate: Nat3 = this

  type Zero = Zero.type
  case object Zero extends Nat3

  type One = One.type
  case object One extends Nat3

  type Two = Two.type
  case object Two extends Nat3

  case class Addition(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Nat3 =
      (left.evaluate, right.evaluate) match
        case (Zero, value)           => value
        case (value, Zero)           => value
        case (One, One)              => Two
        case (One, Two) | (Two, One) => Zero
        case (Two, Two)              => One

    override def toString: String = s"($left + $right)"

  case class Multiplication(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Nat3 =
      (left.evaluate, right.evaluate) match
        case (Zero, _) | (_, Zero) => Zero
        case (One, value)          => value
        case (value, One)          => value
        case (Two, Two)            => One

    override def toString: String = s"($left * $right)"

  extension (expr: Expression)

    @targetName("Addition")
    infix def +(that: Expression): Addition = Addition(expr, that)

    @targetName("Multiplication")
    infix def *(that: Expression): Multiplication = Multiplication(expr, that)
