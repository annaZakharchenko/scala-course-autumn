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
    expression <- genExpression
  yield Negation(expression)

  val genConjunction: Gen[Conjunction] = for
    left <- genExpression
    right <- genExpression
  yield Conjunction(left, right)

  val genDisjunction: Gen[Disjunction] = for
    left <- genExpression
    right <- genExpression
  yield Disjunction(left, right)

  val genImplication: Gen[Implication] = for
    left <- genExpression
    right <- genExpression
  yield Implication(left, right)

  lazy val genExpression: Gen[Expression] = Gen.oneOf(
    genBoolean,
    genVariable,
    genNegation,
    genConjunction,
    genDisjunction,
    genImplication
  )

  given Arbitrary[Boolean]    =  Arbitrary(genBoolean)
  given Arbitrary[Variable]   = Arbitrary(genVariable)
  given Arbitrary[Expression] = Arbitrary(genExpression)

