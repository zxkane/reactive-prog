package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      namedExpressions.mapValues { x => Var(eval(x(), namedExpressions)) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalInternal(expr: Expr, references: Map[String, Signal[Expr]], usedRefs: Set[String]): Double = {
       expr match {
            case Literal(v : Double) => v
            case Ref(name: String) => {
              if (usedRefs.contains(name))
                Double.NaN
              else
                evalInternal(getReferenceExpr(name, references), references, usedRefs + name)
            }
            case Plus(a: Expr, b: Expr) => evalInternal(a, references, usedRefs) + evalInternal(b, references, usedRefs)
            case Minus(a: Expr, b: Expr) => evalInternal(a, references, usedRefs) - evalInternal(b, references, usedRefs)
            case Times(a: Expr, b: Expr) => evalInternal(a, references, usedRefs) * evalInternal(b, references, usedRefs)
            case Divide(a: Expr, b: Expr) => evalInternal(a, references, usedRefs) / evalInternal(b, references, usedRefs)
            case _ => Double.NaN
      }
    } 
    evalInternal(expr, references, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
