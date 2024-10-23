package jsy.lab4

import scala.util.parsing.input.Positional

/** @author
  *   Bor-Yuh Evan Chang
  */
object ast {
  sealed trait Expr extends Positional // e ::=

  /* Literals */
  case class N(n: Double) extends Expr // e ::= n
  case class B(b: Boolean) extends Expr // e ::= b
  case class S(str: String) extends Expr // e ::= str
  case object Undefined extends Expr // e ::= undefined

  /* Unary and Binary Expressions */
  case class Unary(uop: Uop, e1: Expr) extends Expr // e ::= uop e1
  case class Binary(bop: Bop, e1: Expr, e2: Expr) extends Expr // e ::= e1 bop e2

  sealed trait Uop
  case object Neg extends Uop /* -e1 */
  case object Not extends Uop /* !e1 */

  sealed trait Bop
  /* Numbers */
  case object Plus extends Bop /* e1 + e2 */
  case object Minus extends Bop /* e1 - e2 */
  case object Times extends Bop /* e1 * e2 */
  case object Div extends Bop /* e1 / e2 */
  case object Eq extends Bop /* e1 === e2 */
  case object Ne extends Bop /* e1 !=== e2 */
  case object Lt extends Bop /* e1 < e2 */
  case object Le extends Bop /* e1 <= e2 */
  case object Gt extends Bop /* e1 > e2 */
  case object Ge extends Bop /* e1 >= e2 */
  /* Booleans */
  case object And extends Bop /* e1 && e2 */
  case object Or extends Bop /* e1 || e2 */

  /* Intraprocedural Control */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr // e ::= e1 ? e2 : e3

  /* Side-Effects */
  case class Print(e1: Expr) extends Expr // e ::= console.log(e1)
  case object Seq extends Bop // bop ::= ,

  /* Variables */
  case class Var(x: String) extends Expr // e ::= x
  case class ConstDecl(x: String, e1: Expr, e2: Expr) extends Expr // e ::= const x = e1; e2

  /* Functions */
  case class Fun(
    xopt: Option[String],
    yts: List[(String, Typ)],
    tretopt: Option[Typ],
    e1: Expr
  ) extends Expr // e::= xopt(yts)tretopt => e1
  case class Call(e0: Expr, es: List[Expr]) extends Expr // e ::= e1([args])

  /* Objects */
  case class Obj(fes: Map[String, Expr]) extends Expr
  case class GetField(e1: Expr, f: String) extends Expr

  /* Types */
  sealed trait Typ
  case object TNumber extends Typ
  case object TBool extends Typ
  case object TString extends Typ
  case object TUndefined extends Typ
  case class TFun(params: List[(String, Typ)], tret: Typ) extends Typ {
    override def equals(other: Any) = other.isInstanceOf[TFun] && {
      other match {
        case TFun(oparams, otret)
            if otret == tret && oparams.length == params.length =>
          (oparams zip params).forall { case ((_, omt), (_, mt)) => omt == mt }
        case _ => false
      }
    }
  }
  case class TObj(fts: Map[String, Typ]) extends Typ

  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case N(_) | B(_) | Undefined | S(_) | Fun(_, _, _, _)               => true
    case Obj(fields) if (fields forall { case (_, ei) => isValue(ei) }) => true
    case _                                                              => false
  }

  /*
   * Pretty-print values.
   *
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n)      => n.toString
      case B(b)      => b.toString
      case Undefined => "undefined"
      case S(s)      => s
      case Fun(p, _, _, _) =>
        "[Fun%s]".format(p match {
          case None    => ""
          case Some(s) => ": " + s
        })
      case Obj(fields) =>
        val pretty_fields =
          fields map {
            case (f, S(s)) => f + ": '" + s + "'"
            case (f, v)    => f + ": " + pretty(v)
          } reduceRightOption { (s, acc) =>
            s + ",\n  " + acc
          }
        "{ %s }".format(pretty_fields.getOrElse(""))
    }
  }

  /*
   * Pretty-print types.
   *
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(t: Typ): String = t match {
    case TNumber    => "number"
    case TBool      => "bool"
    case TString    => "string"
    case TUndefined => "Undefined"
    case TFun(params, tret) => {
      val pretty_params =
        params map { case (x, mt) =>
          "%s: %s".format(x, pretty(mt))
        } reduceRightOption { (s, acc) =>
          s + ", " + acc
        }
      "(%s) => %s".format(pretty_params.getOrElse(""), pretty(tret))
    }
    case TObj(tfields) =>
      val pretty_fields: Option[String] =
        tfields map { case (f, t) =>
          "%s: %s".format(f, pretty(t))
        } reduceRightOption { (s, acc) =>
          s + "; " + acc
        }
      "{ %s }".format(pretty_fields.getOrElse(""))
  }


  /* Get the free variables of e. */
  def freeVarsVar(e: Expr): Set[Var] = {
    def fv(e: Expr): Set[Var] = e match {
      case vr @ Var(x)        => Set(vr)
      case ConstDecl(x, e1, e2) => fv(e1) | (fv(e2) - Var(x))
      case Fun(p, params, _, e1) => {
        val boundvars = (params map { case (x, _) => Var(x) }) ++ (p map Var)
        fv(e1) -- boundvars
      }
      case N(_) | B(_) | Undefined | S(_) => Set.empty
      case Unary(_, e1)                   => fv(e1)
      case Binary(_, e1, e2)              => fv(e1) | fv(e2)
      case If(e1, e2, e3)                 => fv(e1) | fv(e2) | fv(e3)
      case Call(e1, args) =>
        fv(e1) | args.foldLeft(Set.empty: Set[Var]) {
          ((acc: Set[Var], ei) => acc | fv(ei))
        }
      case Print(e1) => fv(e1)
      case Obj(fields) =>
        fields.foldLeft(Set.empty: Set[Var])({ case (acc, (_, ei)) =>
          acc | fv(ei)
        })
      case GetField(e1, _) => fv(e1)
    }
    fv(e)
  }
  def freeVars(e: Expr): Set[String] = freeVarsVar(e) map { case Var(x) => x }

  /* Check closed expressions. */
  def closed(e: Expr): Boolean = freeVarsVar(e).isEmpty
  def checkClosed(e: Expr): Unit = {
    freeVarsVar(e).headOption.foreach { x => throw new UnboundVariableError(x) }
  }

  /*
   * Unbound Variable Error exception. Throw this exception to signal an unbound variable.
   */
  case class UnboundVariableError(x: Var) extends Exception {
    override def toString =
      Parser.formatErrorMessage(
        x.pos,
        "UnboundVariableError",
        "unbound variable %s".format(x.x)
      )
  }

  /*
   * Static Type Error exception.  Throw this exception to signal a static
   * type error.
   *
   *   throw StaticTypeError(tbad, esub, e)
   *
   */
  case class StaticTypeError(tbad: Typ, esub: Expr, e: Expr) extends Exception {
    override def toString =
      Parser.formatErrorMessage(
        esub.pos,
        "StaticTypeError",
        "invalid type %s for sub-expression %s in %s".format(
          pretty(tbad),
          esub,
          e
        )
      )
  }

  /*
   * Stuck Error exception.  Throw this exception to signal getting
   * stuck in evaluation.  This exception should not get raised if
   * evaluating a well-typed expression.
   *
   *   throw StuckError(e)
   *
   */
  case class StuckError(e: Expr) extends Exception {
    override def toString =
      Parser.formatErrorMessage(e.pos, "StuckError", "in evaluating " + e)
  }

  /*
   * Termination Error exception. Throw this exception to signal exceeding maximum number of allowed steps.
   */
  case class TerminationError(e: Expr, n: Int) extends Exception {
    override def toString = Parser.formatErrorMessage(
      e.pos,
      "TerminationError",
      s"exceeded ${n} steps in evaluating ${e}"
    )
  }
}
