package mexmath.foundation.week1.homework

import mexmath.foundation.week1.homework.booleans.*
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object arbitraries:

  val genBoolean: Gen[Boolean] = Gen.oneOf(True, False)

  val genNegation: Gen[Negation] =
    for expression <- genExpression
    yield Negation(expression)

  //val genConjunction: Gen[Conjunction] = ???
  val genConjunction: Gen[Conjunction] =
    for 
      left <- genExpression
      right <- genExpression
    yield Conjunction(left, right)
    
  //val genDisjunction: Gen[Disjunction] = ???
  val genDisjunction: Gen[Disjunction] =
    for
      left <- genExpression
      right <- genExpression
    yield Disjunction(left, right)
    
  //val genImplication: Gen[Implication] = ???
  val genImplication: Gen[Implication] =
    for
      left <- genExpression
      right <- genExpression
    yield Implication(left, right)
    
  //lazy val genExpression: Gen[Expression] = ???
  lazy val genExpression: Gen[Expression] =
    Gen.oneOf(genBoolean, genNegation, genConjunction, genDisjunction, genImplication)
    
  //given Arbitrary[Boolean]    = ???
  given Arbitrary[Boolean] = Arbitrary(genBoolean) 
  //given Arbitrary[Expression] = ???
  given Arbitrary[Expression] = Arbitrary(genExpression)