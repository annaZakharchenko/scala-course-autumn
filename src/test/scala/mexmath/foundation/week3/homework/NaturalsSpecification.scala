package mexmath.foundation.week3.homework

import mexmath.foundation.week3.homework.naturals.{Nat, Succ, Zero}
import mexmath.foundation.week3.homework.arbitraries.given 
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean

object NaturalsSpecification extends Properties("Naturals"):

  property("Addition is commutative") = forAll { (n1: Nat, n2: Nat) =>
    (n1 + n2).toInt == (n2 + n1).toInt
  }

  property("Addition is associative") = forAll { (n1: Nat, n2: Nat, n3: Nat) =>
    ((n1 + n2) + n3).toInt == (n1 + (n2 + n3)).toInt
  }

  property("Zero is neutral for addition") = forAll { (n: Nat) =>
    (n + Zero).toInt == n.toInt
  }

  property("Subtracting zero does not change value") = forAll { (n: Nat) =>
    (n - Zero).toInt == n.toInt
  }

end NaturalsSpecification

object ZeroSpecification extends Properties("Zero"):

  property("Zero is always zero") = forAll { (_: Zero) =>
    Zero.isZero && Zero.toInt == 0
  }

  property("Zero successor is not zero") = forAll { (_: Zero) =>
    Zero.successor.isZero == false
  }

  property("Adding Zero does not change other value") = forAll { (n: Nat) =>
    (Zero + n).toInt == n.toInt
  }

  property("Subtracting Zero from Zero is Zero") = forAll { (_: Zero) =>
    (Zero - Zero).toInt == 0
  }

end ZeroSpecification

object SuccSpecification extends Properties("Succ"):

  property("Successor increases value by 1") = forAll { (n: Nat) =>
    val succ = n.successor
    succ.toInt == n.toInt + 1
  }

  property("Predecessor decreases value by 1") = forAll { (n: Nat) =>
    !n.isZero ==> {
      val pred = n.predecessor
      pred.toInt == n.toInt - 1
    }
  }

  property("Successor of Zero is not Zero") = forAll { (_: Zero) =>
    Zero.successor.isZero == false
  }

  property("Adding a successor equals adding one") = forAll { (n1: Nat, n2: Nat) =>
    (n1 + n2.successor).toInt == (n1 + n2).toInt + 1
  }

end SuccSpecification

object NatSpecification extends Properties("Nat") {

  property("toInt is consistent with successor") = forAll { (n: Nat) =>
    n.successor.toInt == n.toInt + 1
  }

  property("Subtraction produces Zero when both numbers are equal") = forAll { (n: Nat) =>
    (n - n).isZero
  }

  property("Subtracting smaller from larger gives valid Nat") = forAll { (n1: Nat, n2: Nat) =>
    (n1.toInt >= n2.toInt) ==> {
      val result = n1 - n2
      result.toInt == n1.toInt - n2.toInt
    }
  }

  property("Predecessor followed by successor gives the same value") = forAll { (n: Nat) =>
    !n.isZero ==> {
      val pred = n.predecessor
      val succ = pred.successor
      succ.toInt == n.toInt
    }
  }
}
end NatSpecification
