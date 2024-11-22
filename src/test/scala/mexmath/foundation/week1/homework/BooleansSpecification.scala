package mexmath.foundation.week1.homework

import mexmath.foundation.week1.homework.arbitraries.given
import mexmath.foundation.week1.homework.booleans.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Booleans"):

  include(NegationSpecification)
  include(ConjunctionSpecification)
  include(DisjunctionSpecification)
  include(ImplicationSpecification)
  include(AxiomsSpecification)

end BooleansSpecification

object NegationSpecification extends Properties("Negation"):

  property("!True is False") = propBoolean {
    (!True).evaluate == False
  }

  property("!False is True") = ???

end NegationSpecification

object ConjunctionSpecification extends Properties("Conjunction"):

  property("True ∧ value is value") = forAll { (value: Boolean) =>
    ???
  }

  property("False ∧ value is False") = ???

end ConjunctionSpecification

object DisjunctionSpecification extends Properties("Disjunction"):

  property("True ∨ value is True") = ???

  property("False ∨ value is value") = ???

end DisjunctionSpecification

object ImplicationSpecification extends Properties("Implication"):

  property("True → value is value") = ???

  property("False → value is True") = ???

end ImplicationSpecification

object AxiomsSpecification extends Properties("Axioms"):

  property("a → (b → a)") = ???

  property("(a → (b → c)) → ((a → b) → (a → c))") = ???

  property("(a ∧ b) → a") = ???

  property("(a ∧ b) → b") = ???

  property("a → (b → (a ∧ b))") = ???

  property("a → (a ∨ b)") = ???

  property("b → (a ∨ b)") = ???

  property("(a → c) → ((b → c) → ((a ∨ b) → c))") = ???

  property("!a → (a → b)") = ???

  property("(a → b) → ((a → !b) → !a)") = ???

  property("a ∨ !a") = ???

end AxiomsSpecification
