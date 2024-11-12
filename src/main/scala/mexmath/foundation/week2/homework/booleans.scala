package mexmath.foundation.week2.homework

import scala.annotation.targetName

object booleans:

  sealed trait Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  sealed trait Boolean extends Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  case object True extends Boolean:
    def evaluate: Expression                                               = this
    def substitute(variable: Variable, expression: Expression): Expression = this

  case object False extends Boolean:
    def evaluate: Expression                                               = this
    def substitute(variable: Variable, expression: Expression): Expression = this

  case class Variable(name: String) extends Expression:
    def evaluate: Expression = this

    def substitute(variable: Variable, expression: Expression): Expression =
      if this == variable then expression
      else this

    override def toString: String = name

  case class Negation(expression: Expression) extends Expression:
    def evaluate: Expression = expression.evaluate match {
      case True  => False
      case False => True
      case expr  => Negation(expr)
    }

    def substitute(variable: Variable, expression: Expression): Expression =
      Negation(this.expression.substitute(variable, expression))

    override def toString: String = s"!(${expression.toString})"

  case class Conjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression = (left.evaluate, right.evaluate) match {
      case (True, True) => True
      case _            => False
    }

    def substitute(variable: Variable, expression: Expression): Expression =
      Conjunction(left.substitute(variable, expression), right.substitute(variable, expression))

    override def toString: String = s"(${left.toString} ∧ ${right.toString})"

  case class Disjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression =
      if left.evaluate == True then True
      else right.evaluate

    def substitute(variable: Variable, expression: Expression): Expression =
      Disjunction(left.substitute(variable, expression), right.substitute(variable, expression))

    override def toString: String = s"(${left.toString} ∨ ${right.toString})"

  case class Implication(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression = (left.evaluate, right.evaluate) match {
      case (True, False) => False
      case _             => True
    }

    def substitute(variable: Variable, expression: Expression): Expression =
      Implication(left.substitute(variable, expression), right.substitute(variable, expression))

    override def toString: String = s"(${left.toString} → ${right.toString})"

  given Conversion[String, Variable] with
    def apply(str: String): Variable = Variable(str)

  extension (expr: Expression)

    @targetName("Negation")
    infix def unary_! : Negation = Negation(expr)

    @targetName("Conjunction")
    infix def ∧(that: Expression): Conjunction = Conjunction(expr, that)

    @targetName("Disjunction")
    infix def ∨(that: Expression): Disjunction = Disjunction(expr, that)

    @targetName("Implication")
    infix def →(that: Expression): Implication = Implication(expr, that)
