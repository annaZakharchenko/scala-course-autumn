package mexmath.foundation.week1.homework

import scala.annotation.targetName

object booleans:

  sealed trait Expression:
    def evaluate: Boolean

  sealed trait Boolean extends Expression:
    val evaluate: Boolean = this

  type True = True.type
  case object True extends Boolean
  
  type False = False.type
  case object False extends Boolean

  case class Negation(expression: Expression) extends Expression:
    def evaluate: Boolean = expression.evaluate match
      case False => True // true
      case True => False // false
    override def toString: String = s"!(${expression.toString})"

  // Provide implementation for `Conjunction` type
  //type Conjunction
  case class Conjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Boolean = (left.evaluate, right.evaluate) match
      case (True, True) => True
      case _ => False // other cases((True, False) ,(False, True) and (False, False)) will return False
    override def toString: String = s"(${left.toString} ∧ ${right.toString})"  

  // Provide implementation for `Disjunction` type
  //type Disjunction
  case class Disjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Boolean =
      if left.evaluate == True then True
      else right.evaluate
    override def toString: String = s"(${left.toString} ∨ ${right.toString}) "

  // Provide implementation for `Implication` type
  //type Implication
  case class Implication(left: Expression, right: Expression) extends Expression:
    def evaluate: Boolean = (left.evaluate, right.evaluate) match
      case (True, False) => False
      case _ => True // other cases ((False, False), (False, True) and(True, True)) return True 
    override def toString: String = s"(${left.toString} → ${right.toString}) "

  def transformImplications(expr: Expression): Expression = expr match {
    case True => True
    case False => False
    case Negation(inner) =>
      Negation(transformImplications(inner))
    case Conjunction(left, right) =>
      Conjunction(transformImplications(left), transformImplications(right))
    case Disjunction(left, right) =>
      Disjunction(transformImplications(left), transformImplications(right))
    case Implication(left, right) =>
      Disjunction(Negation(transformImplications(left)), transformImplications(right))
  }  

  extension (expr: Expression)

    @targetName("Negation")
    infix def unary_! : Negation = Negation(expr)

    @targetName("Conjunction")
    infix def ∧(that: Expression): Conjunction = Conjunction(expr, that)

    @targetName("Disjunction")
    infix def ∨(that: Expression): Disjunction = Disjunction(expr, that)

    @targetName("Implication")
    infix def →(that: Expression): Implication = Implication(expr, that)


    