package mexmath.foundation.week3.homework

import mexmath.foundation.week3.homework.naturals.{Nat, Succ, Zero}


object naturals:

  abstract class Nat {
    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat = Succ(this)

    infix def +(that: Nat): Nat

    infix def -(that: Nat): Nat

    def toInt: Int = {
      if (this.isZero) 0
      else 1 + predecessor.toInt
    }

    override def toString: String = s"Nat($predecessor)"
  }

  type Zero = Zero.type

  object Zero extends Nat {
    def isZero: Boolean = true

    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat = if (that.isZero) this else throw new Exception("Negative numbers not allowed")

    override def toInt: Int = 0

    override def toString: String = "Zero"
  }

  class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false

    def predecessor: Nat = n

    infix def +(that: Nat): Nat = Succ(n + that)

    infix def -(that: Nat): Nat = if (that.isZero) this else n - (that.predecessor)

    override def toInt: Int = 1 + n.toInt

    override def toString: String = s"Succ($n)"
  }
