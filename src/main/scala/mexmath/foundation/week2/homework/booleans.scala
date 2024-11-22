package mexmath.foundation.week2.homework

import scala.annotation.targetName

object booleans:

  sealed trait Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  sealed trait Boolean extends Expression:
    val evaluate: Expression                                               = this
    def substitute(variable: Variable, expression: Expression): Expression = ???

  type True = True.type
  case object True extends Boolean

  type False = False.type
  case object False extends Boolean

  type Variable

  case class Negation(expression: Expression) extends Expression:

    def evaluate: Expression                                               = ???
    def substitute(variable: Variable, expression: Expression): Expression = ???

    override def toString: String = ???

  // Provide implementation for `Conjunction` type
  type Conjunction

  // Provide implementation for `Disjunction` type
  type Disjunction

  // Provide implementation for `Implication` type
  type Implication

  // Add conversion from `String` to `Variable`

  extension (expr: Expression)

    @targetName("Negation")
    infix def unary_! : Negation = Negation(expr)

    @targetName("Conjunction")
    infix def ∧(that: Expression): Conjunction = ???

    @targetName("Disjunction")
    infix def ∨(that: Expression): Disjunction = ???

    @targetName("Implication")
    infix def →(that: Expression): Implication = ???
