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

  property("!False is True") = propBoolean{
    (!False).evaluate == True
  }

end NegationSpecification

object ConjunctionSpecification extends Properties("Conjunction"):

  property("True ∧ value is value") = forAll { (value: Boolean) =>
    (True ∧ value).evaluate == value
  }

  property("False ∧ value is False") = forAll: (value: Boolean) =>
    (False ∧ value).evaluate == False

end ConjunctionSpecification

object DisjunctionSpecification extends Properties("Disjunction"):

  property("True ∨ value is True") =  forAll: (value: Boolean) =>
    (True ∨ value).evaluate == True


  property("False ∨ value is value") = forAll: (value: Boolean) =>
    (False ∨ value).evaluate == value

end DisjunctionSpecification

object ImplicationSpecification extends Properties("Implication"):

  property("True → value is value") = forAll: (value: Boolean) =>
    (False → value).evaluate == True

  property("False → value is True") = forAll: (value: Boolean) =>
    (True → value).evaluate == value

end ImplicationSpecification

val expressionGen: Gen[Expression] =
  Gen.oneOf(True, False)

object AxiomsSpecification extends Properties("Axioms"):

  property("a → (b → a)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => (a → (b → a)).evaluate == True

  property("(a → (b → c)) → ((a → b) → (a → c))") = forAll(expressionGen, expressionGen, expressionGen):
    (a: Expression, b: Expression, c: Expression) => ((a → (b → c)) → ((a → b) → (a → c))).evaluate == True

  property("(a ∧ b) → a") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => ((a ∧ b) → a).evaluate == True

  property("(a ∧ b) → b") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => ((a ∧ b) → b).evaluate == True

  property("a → (b → (a ∧ b))") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => (a → (b → (a ∧ b))).evaluate == True

  property("a → (a ∨ b)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => (a → (a ∨ b)).evaluate == True

  property("b → (a ∨ b)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => (b → (a ∨ b)).evaluate == True

  property("(a → c) → ((b → c) → ((a ∨ b) → c))") = forAll(expressionGen, expressionGen, expressionGen):
    (a: Expression, b: Expression, c: Expression) => ((a → c) → ((b → c) → ((a ∨ b) → c))).evaluate == True

  property("!a → (a → b)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => (!a → (a → b)).evaluate == True

  property("(a → b) → ((a → !b) → !a)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) => ((a → b) → ((a → !b) → !a)).evaluate == True

  property("a ∨ !a") = forAll(expressionGen):
    (a: Expression) => (a ∨ !a).evaluate == True

object BooleanSubstitutionSpecification extends Properties("Boolean Substitution"):

  property("substitution into Nat should make no changes") = forAll { (b: Boolean, v: Variable) =>
    b.substitute(v, b) == b
  }

end BooleanSubstitutionSpecification

object VariableSubstitutionSpecification extends Properties("Variable Substitution"):

  property("substitution into different variable should make no changes") = forAll { (v1: Variable, v2: Variable, expr: Expression) =>
    expr.substitute(v1, True) == expr
  }

  property("substitution into the same variable should return the given expression") = forAll { (v: Variable, expr: Expression) =>
    expr.substitute(v, True) == True
  }

end VariableSubstitutionSpecification

object ExpressionSubstitutionSpecification extends Properties("Expression Substitution"):

  property("substitution into !expression should be equal to !(substitution into expression)") = forAll { (expr: Expression, v: Variable, replacement: Expression) =>
    (!expr).substitute(v, replacement) == !expr.substitute(v, replacement)
  }

  property("substitution into left ∧ right should be equal to substitution into left ∧ substitution into right") =  forAll { (left: Expression, right: Expression, v: Variable, replacement: Expression) =>
    (left ∧ right).substitute(v, replacement) == left.substitute(v, replacement) ∧ right.substitute(v, replacement)
  }

  property("substitution into left ∨ right should be equal to substitution into left ∨ substitution into right") =  forAll { (left: Expression, right: Expression, v: Variable, replacement: Expression) =>
    (left ∧ right).substitute(v, replacement) == left.substitute(v, replacement) ∧ right.substitute(v, replacement)
  }

  property("substitution into left → right should be equal to substitution into left → substitution into right") = forAll { (left: Expression, right: Expression, v: Variable, replacement: Expression) =>
    (left → right).substitute(v, replacement) == left.substitute(v, replacement) → right.substitute(v, replacement)
  }

end ExpressionSubstitutionSpecification
