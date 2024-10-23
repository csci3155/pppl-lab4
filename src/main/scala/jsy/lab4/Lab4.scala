package jsy.lab4

object Lab4 extends jsy.util.JsyApplication {
  import ast._

  /*
   * CSCI 3155: Lab 4
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /* Helper functions */

  // You should have implemented this already in HW 4
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil    => ???
    case h :: t => ???
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * You may use this function or ignore it.
   */
  def doInequality(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"doInequality: v1 ${v1} is not a value")
    require(isValue(v2), s"doInequality: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }

  /* Small-Step Interpreter */

  /* Scope-respecting substitution substituting v for free uses of variable x iin e. */
  def substitute(v: Expr, x: String, e: Expr): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1)                      => Print(subst(e1))
      case Unary(uop, e1)                 => ???
      case Binary(bop, e1, e2)            => ???
      case If(e1, e2, e3)                 => ???
      case Var(y)                         => ???
      case ConstDecl(y, e1, e2)           => ???
      case Fun(xopt, yts, tretopt, e1)    => ???
      case Call(e0, es)                   => ???
      case Obj(fields)                    => ???
      case GetField(e1, f)                => ???
    }
    subst(e)
  }

  /* Type Inference */

  type TEnv = Map[String, Typ]
  def lookup[A](env: Map[String, A], x: String): A = env(x)
  def extend[A](env: Map[String, A], x: String, a: A): Map[String, A] = {
    env + (x -> a)
  }

  def hastype(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      /* Base TypeScripty */
      // TypePrint
      case Print(e1) => hastype(env, e1); TUndefined
      // TypeNumber
      case N(_) => ???
      // TypeBool
      case B(_) => ???
      // TypeUndefined
      case Undefined => ???
      // TypeString
      case S(_) => ???
      // TypeVar
      case Var(x) => ???
      // TypNeg
      case Unary(Neg, e1) =>
        hastype(env, e1) match {
          case TNumber => TNumber
          case tgot    => err(tgot, e1)
        }
      // TypeNot
      case Unary(Not, e1) =>
        ???
      // TypeArith for + and TypePlusString
      case Binary(Plus, e1, e2) =>
        ???
      // TypeArith for -, *, and /
      case Binary(Minus | Times | Div, e1, e2) =>
        ???
      // TypeEquality
      case Binary(Eq | Ne, e1, e2) =>
        ???
      // TypeInequalityNumber and TypeInequalityString
      case Binary(Lt | Le | Gt | Ge, e1, e2) =>
        ???
      // TypeAndOr
      case Binary(And | Or, e1, e2) =>
        ???
      // TypeSeq
      case Binary(Seq, e1, e2) =>
        ???
      // TypeIf
      case If(e1, e2, e3) =>
        ???
      // TypeConstDecl
      case ConstDecl(x, e1, e2) => ???

      /* Immutable Objects */
      // TypeObject
      case Obj(fields) => ???
      // TypeGetField
      case GetField(e1, f) => ???

      /* Multi-Parameter Functions */
      // TypeFunction, TypeFunctionAnn, and TypeFunctionRec
      case Fun(xopt, yts, tretopt, e1) => {
        /* Here is a possible structure to handle TypeFunction, TypeFunctionAnn, and TypeFunctionRec together.
         * You are welcome to use this or ignore it. */

        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (xopt, tretopt) match {
          // ... add cases here
          case _ => ???
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = ???
        // Infer the type of the function body
        val t1 = ???
        // Check with the possibly annotated return type
        ???
      }
      // TypeCall
      case Call(e0, args) =>
        hastype(env, e0) match {
          case TFun(params, tret) if (params.length == args.length) =>
            /* Here is a possible structure.
             * You are welcome to use this or ignore it.
             */
            (params zip args).foreach {
              ???
            }
            tret
          case tgot => err(tgot, e0)
        }
    }
  }

  /* A small-step transition. */
  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */

      /* Base TypeScripty */
      // DoPrint
      case Print(v1) if isValue(v1)      => println(pretty(v1)); Undefined
      // DoNeg
      case Unary(Neg, v1) if isValue(v1) => ???
      // ... additional cases

      /* Immutable Objects  */
      // DoGetField
      case GetField(v @ Obj(fields), f) if isValue(v) => ???

      /* Multi-Parameter Functions */
      // DoCall and DoCallRec
      case Call(v @ Fun(xopt, params, _, e), args) if args.forall(isValue) => {
        /* Here is a possible structure to handle DoCall and DoCallRec together.
         * You are welcome to use this or ignore it. */
        val pazip = params zip args
        val ep = pazip.foldRight(e) {
          ???
        }
        xopt match {
          case None    => ???
          case Some(x) => ???
        }
      }

      /* Inductive Cases: Search Rules */

      /* Base TypeScripty */
      // SearchPrint
      case Print(e1)      => Print(step(e1))
      // SearchUnary
      case Unary(uop, e1) => ???
      // ... additional cases

      /* Immutable Objects */
      // SearchGetField
      case GetField(e1, f) => ???
      // SearchObject
      case Obj(fields) => {
        val Some((fi, ei)) = fields find { case (_, ei) => !isValue(ei) }
        ???
      }

      /* Multi-Parameter Functions */
      // SearchCall2
      case Call(v, args) if isValue(v) => ???
      // SearchCall1
      case Call(e, args)               => ???

      /* Everything else is a stuck error. Should not happen if e is well-typed. */
      case _ => throw StuckError(e)
    }
  }

  /* External Interfaces */

  /** Interface to run your type checker. */
  def inferType(e: Expr): Typ = {
    val t = hastype(Map.empty, e)
    println(s"## ${e} : ${pretty(t)}")
    t
  }

  /** Interface to run your small-step interpreter and print out the steps of
    * evaluation if debugging.
    */
  def iterateStep(e: Expr): Expr = {
    require(
      closed(e),
      "input Expr to iterateStep is not closed: free variables: %s".format(
        freeVars(e)
      )
    )
    def loop(e: Expr, n: Int): Expr = {
      if (Some(n) == maxSteps) throw TerminationError(e, n)
      if (isValue(e)) e
      else {
        println("## step %4d: %s".format(n, e))
        loop(step(e), n + 1)
      }
    }
    val v = loop(e, 0)
    println(s"## value: %s".format(v))
    v
  }

  /** Interface to type check from a string. This is convenient for unit
    * testing.
    */
  def inferType(s: String): Typ = inferType(Parser.parse(s))

  /** Interface to take a small-step from a string. This is convenient for unit
    * testing.
    */
  def oneStep(s: String): Expr = step(Parser.parse(s))

  /** Interface to run your small-step interpreter from a string. This is
    * convenient for unit testing.
    */
  def iterateStep(s: String): Expr = iterateStep(Parser.parse(s))

  // Interface for main

  //this.debug = true // uncomment this if you want to print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

  def processFile(file: java.io.File): Unit = {
    if (debug) {
      println("# ============================================================")
      println("# File: " + file.getName)
      println("# Parsing ...")
    }

    val exprin =
      handle(None: Option[Expr]) {
        Some {
          Parser.parseFile(file)
        }
      } getOrElse {
        return
      }

    val expr = exprin

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Type checking %s ...".format(expr))
    }

    val welltyped = handle(false) {
      val t = inferType(expr)
      true
    }
    if (!welltyped) return

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Stepping ...".format(expr))
    }

    handle(()) {
      val v = iterateStep(expr)
      println(pretty(v))
    }
  }

}
