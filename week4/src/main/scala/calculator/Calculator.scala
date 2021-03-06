package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(expr => Signal(eval(expr(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) =>
        val refExpr = getReferenceExpr(name, references)
        eval(refExpr, references - name)
      case Plus(a, b) => if ((a != expr) && (b != expr)) eval(a, references) + eval(b, references) else Double.NaN
      case Minus(a, b) => if ((a != expr) && (b != expr)) eval(a, references) - eval(b, references) else Double.NaN
      case Times(a, b) => if ((a != expr) && (b != expr)) eval(a, references) * eval(b, references) else Double.NaN
      case Divide(a, b) => if ((a != expr) && (b != expr)) eval(a, references) / eval(b, references) else Double.NaN
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
