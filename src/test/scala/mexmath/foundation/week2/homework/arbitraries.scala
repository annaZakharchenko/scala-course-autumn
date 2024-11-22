package mexmath.foundation.week2.homework

import mexmath.foundation.week2.homework.booleans.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object arbitraries:

  val genBoolean: Gen[Boolean] = Gen.oneOf(True, False)

  val genVariableName: Gen[String] = ???

  val genVariable: Gen[Variable] = ???

  val genNegation: Gen[Negation] =
    for expression <- genExpression
    yield Negation(expression)

  val genConjunction: Gen[Conjunction] = ???

  val genDisjunction: Gen[Disjunction] = ???

  val genImplication: Gen[Implication] = ???

  lazy val genExpression: Gen[Expression] = ???

  given Arbitrary[Boolean]    = ???
  given Arbitrary[Variable]   = ???
  given Arbitrary[Expression] = ???
