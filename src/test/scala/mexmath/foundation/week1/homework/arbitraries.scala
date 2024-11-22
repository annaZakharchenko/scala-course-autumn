package mexmath.foundation.week1.homework

import mexmath.foundation.week1.homework.booleans.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object arbitraries:

  val genBoolean: Gen[Boolean] = Gen.oneOf(True, False)

  val genNegation: Gen[Negation] =
    for expression <- genExpression
    yield Negation(expression)

  val genConjunction: Gen[Conjunction] = ???

  val genDisjunction: Gen[Disjunction] = ???

  val genImplication: Gen[Implication] = ???

  lazy val genExpression: Gen[Expression] = ???

  given Arbitrary[Boolean]    = ???
  given Arbitrary[Expression] = ???
