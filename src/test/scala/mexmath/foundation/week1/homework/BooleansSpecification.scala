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
  include(TransformationSpecification)

end BooleansSpecification

object NegationSpecification extends Properties("Negation"):

  property("!True is False") = propBoolean:
    (!True).evaluate == False


  property("!False is True") = propBoolean:
    (!False).evaluate == True

end NegationSpecification

object ConjunctionSpecification extends Properties("Conjunction"):

  property("True ∧ value is value") = forAll: (value: Boolean) =>
    (True ∧ value).evaluate == value


  property("False ∧ value is False") = forAll: (value: Boolean) =>
    (False ∧ value).evaluate == False

end ConjunctionSpecification

object DisjunctionSpecification extends Properties("Disjunction"):

  property("True ∨ value is True") = forAll: (value: Boolean) =>
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

  property("a → (b → a)") =  forAll(expressionGen, expressionGen): 
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

end AxiomsSpecification


object TransformationSpecification extends Properties("Transformation"):
  property("a → (b → a)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = a → (b → a)
      transformImplications(expr).evaluate == True

  property("(a → (b → c)) → ((a → b) → (a → c))") = forAll(expressionGen, expressionGen, expressionGen):
    (a: Expression, b: Expression, c: Expression) =>
      val expr = (a → (b → c)) → ((a → b) → (a → c))
      transformImplications(expr).evaluate == True

  property("(a ∧ b) → a") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = (a ∧ b) → a
      transformImplications(expr).evaluate == True

  property("(a ∧ b) → b") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = (a ∧ b) → b
      transformImplications(expr).evaluate == True

  property("a → (b → (a ∧ b))") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = a → (b → (a ∧ b))
      transformImplications(expr).evaluate == True

  property("a → (a ∨ b)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = a → (a ∨ b)
      transformImplications(expr).evaluate == True

  property("b → (a ∨ b)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = b → (a ∨ b)
      transformImplications(expr).evaluate == True

  property("(a → c) → ((b → c) → ((a ∨ b) → c))") = forAll(expressionGen, expressionGen, expressionGen):
    (a: Expression, b: Expression, c: Expression) =>
      val expr = (a → c) → ((b → c) → ((a ∨ b) → c))
      transformImplications(expr).evaluate == True

  property("!a → (a → b)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = !a → (a → b)
      transformImplications(expr).evaluate == True

  property("(a → b) → ((a → !b) → !a)") = forAll(expressionGen, expressionGen):
    (a: Expression, b: Expression) =>
      val expr = (a → b) → ((a → !b) → !a)
      transformImplications(expr).evaluate == True

  property("a ∨ !a") = forAll(expressionGen):
    (a: Expression) =>
      val expr = a ∨ !a
      transformImplications(expr).evaluate == True
end TransformationSpecification
      
      
