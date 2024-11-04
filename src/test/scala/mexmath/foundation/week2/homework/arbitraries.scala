package mexmath.foundation.week2.homework

import mexmath.foundation.week2.homework.booleans.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object arbitraries:

  val genBoolean: Gen[Boolean] = Gen.oneOf(True, False)

  val genVariableName: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  val genVariable: Gen[Variable] = for
    name <- genVariableName
  yield Variable(name)

  val genNegation: Gen[Negation] = for
    expression <- lzy(genExpression)
  yield Negation(expression)

  val genConjunction: Gen[Conjunction] = for
    left <- lzy(genExpression)
    right <- lzy(genExpression)
  yield Conjunction(left, right)

  val genDisjunction: Gen[Disjunction] = for
    left <- lzy(genExpression)
    right <- lzy(genExpression)
  yield Disjunction(left, right)

  val genImplication: Gen[Implication] = for
    left <- lzy(genExpression)
    right <- lzy(genExpression)
  yield Implication(left, right)

  lazy val genExpression: Gen[Expression] = Gen.frequency(
    (1, genBoolean),
    (1, genVariable),
    (1, genNegation),
    (1, genConjunction),
    (1, genDisjunction),
    (1, genImplication)
  )

  given Arbitrary[Boolean]    =  Arbitrary(genBoolean)
  given Arbitrary[Variable]   = Arbitrary(genVariable)
  given Arbitrary[Expression] = Arbitrary(genExpression)

