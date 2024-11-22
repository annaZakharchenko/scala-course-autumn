package mexmath.foundation.week2.homework

import mexmath.foundation.week2.homework.arbitraries.given
import mexmath.foundation.week2.homework.booleans.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Booleans"):

  include(NegationSpecification)
  include(ConjunctionSpecification)
  include(DisjunctionSpecification)
  include(ImplicationSpecification)
  include(AxiomsSpecification)
  include(BooleanSubstitutionSpecification)
  include(VariableSubstitutionSpecification)
  include(ExpressionSubstitutionSpecification)

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

object BooleanSubstitutionSpecification extends Properties("Boolean Substitution"):

  property("substitution into Nat should make no changes") = ???

end BooleanSubstitutionSpecification

object VariableSubstitutionSpecification extends Properties("Variable Substitution"):

  property("substitution into different variable should make no changes") = ???

  property("substitution into the same variable should return the given expression") = ???

end VariableSubstitutionSpecification

object ExpressionSubstitutionSpecification extends Properties("Expression Substitution"):

  property("substitution into !expression should be equal to !(substitution into expression)") = ???

  property("substitution into left ∧ right should be equal to substitution into left ∧ substitution into right") = ???

  property("substitution into left ∨ right should be equal to substitution into left ∨ substitution into right") = ???

  property("substitution into left → right should be equal to substitution into left → substitution into right") = ???

end ExpressionSubstitutionSpecification
