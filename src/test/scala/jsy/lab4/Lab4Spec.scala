package jsy.lab4

import org.scalatest.flatspec.AnyFlatSpec
import Parser.parse
import ast._
import Lab4._

class Lab4StudentSpec extends AnyFlatSpec {
}

class Lab4Spec extends AnyFlatSpec {

  /***** hastype Tests *****/
  {
    val xtype = TNumber
    val tenvx = extend(Map.empty, "x", xtype)

    "TypeVar" should "perform TypeVar" in {
      assertResult(xtype) {
        hastype(tenvx, Var("x"))
      }
    }

    "hastype/call-non-fun" should "yield a static type error" in {
      for (e <- List(
        ("3(4)"),
        ("3(true)"),
        ("(10 + 4 + 7 - true * 3)(true)"),
        ("((function (x: number) { return 0 })(10))(true)")
      )) {
        intercept[StaticTypeError] {
          inferType(e)
        }
      }
    }

    "hastype/arithmetic" should "type check JavaScripty arithmetic expressions according to the lab spec" in {
      for ((e,t) <- List(
        ("3 <= 4", TBool),
        ("'3' < '4'", TBool),
        ("3 + 4", TNumber),
        ("'3' + '4'", TString),
        ("3 - 4", TNumber),
        ("3 * 4 + 1 - 4 / 12", TNumber),
        ("undefined", TUndefined)
      )) {
        assertResult(t){ inferType(e) }
      }
    }
    
    "hastype/functions-objects" should "type check JavaScripty function-object expressions according to the lab spec" in {
      val f1 = parse("function (x: number) { return x }")
      val o1 = parse("{f: 3, g: true, h: undefined}")
      for ((e,t) <- List(
        (f1, TFun(List(("x", TNumber)), TNumber)),
        (o1, TObj(Map("f" -> TNumber, "g" -> TBool, "h" -> TUndefined))),
        (Call(f1, List(N(3))), TNumber),
        (GetField(o1, "g"), TBool)
      )) {
        assertResult(t){ inferType(e) }
      }
    }
    
    "hastype/simple-programs" should "type check JavaScripty programs according to the lab spec" in {
      assertResult(TNumber) { inferType(
        """
        const w = function w(y: number): number { return y === 0 ? 0.1 : y + w(y - 1) };
        w(3)
        """
      )}
      assertResult(TNumber) { inferType(
        """
        const x = 1;
        const g = function(y: number) { return x; };
        const h = function(x: number) { return g(2); };
        h(3)
        """
      )}
    }
    
    "hastype/recursive-programs" should "type check recursive JavaScripty programs according to the lab spec" in {
      assertResult(TNumber) { inferType(
        """
        const factorial = function f(n: number): number {
          return n === 0 ? 1 : n * f(n - 1)
        }
        factorial(4)
        """
      )}
    }
  }


  /***** substitute Tests *****/
  {
    "substitute" should "substitute a value for uses of a variable" in {
      for ((r,e,v,x) <- List(
        ("const x = 3; x", "const x = x; x", "3", "x"),
        ("function (x: number) { return x + y }", "function (x: number) { return x + y }", "3", "x"),
        ("function x(y: number) { return x + y }", "function x(y: number) { return x + y }", "3", "x")
      )) {
        assertResult(parse(r)){ substitute(parse(v), x, parse(e)) }
      }
    }
  }


  /***** step Tests *****/
  {
    "step" should "perform one step of evaluation according to the lab spec" in {
      for ((r, s) <- List (
        ("1", "4 - 3"),
        ("3 + 4", "1, 3 + 4"),
        ("(3) + 3", "(1 + 2) + 3"),
        ("(2) + (3 / 4)", "(1 * 2) + (3 / 4)"),
        ("(2 + 4)", "true ? (2 + 4) : (1 + 1)"),
        ("const x = 2; x", "const x = 1 + 1; x"),
        ("2", "const x = 2; x"),
        ("3 + 3 + 4 + 5", "1 + 2 + 3 + 4 + 5"),
        ("3", "(function (x: number) { return x })(3)"),
        ("3 + 1", "(function (x: number) { return x })(3 + 1)")
      )) {
        assertResult(parse(r)){oneStep(s)}
      }
    } 
  
    "step/call-non-fun" should "yield a stuck error" in {
      for (e <- List(
        ("3(4)"),
        ("3(true)"),
        ("((function (x: number) { return 0 })(10))(true)")
      )) {
        intercept[StuckError] { iterateStep(e) }
      }
    } 
    
    "step/simple-programs" should "evaluate JavaScripty programs according to the lab spec" in {
      assertResult(N(6.1)) { iterateStep(
        """
        const w = function w(y: number) { return y === 0 ? 0.1 : y + w(y - 1) }
        w(3)
        """
      )}
      assertResult(N(1)) { iterateStep(
        """
        const x = 1;
        const g = function(y: number) { return x; };
        const h = function(x: number) { return g(2); };
        h(3)
        """
      )}
      assertResult(N(1.1)) { iterateStep(
        """
        const g = function(x: number) { return function(y: number) { return x - y } };
        g(1)(0) + 0.1
        """
      )}
      assertResult(N(6)) { iterateStep(
        """
        const plus = function(x: number, y: number) { return x + y };
        plus(1 + 2, 3)
        """
      )}
      assertResult(N(6)) { iterateStep(
        """
        const plus = function(x: number) { return function(y: number) { return x + y } };        
        plus(1 + 2)(3)
        """
      )}
    }
    
    "step/recursive-programs" should "evaluate recursive JavaScripty programs according to the lab spec" in {
      assertResult(N(6)) { iterateStep(
        """
        const factorial = function f(n: number) {
          return n === 0 ? 1 : n * f(n - 1)
        }
        factorial(3)
        """
      )}
      assertResult(S("aaa")) { iterateStep(
        """
        const repeat = function(s: string) {
          return function loop(n: number) {
            return n === 0 ? "" : s + loop(n - 1)
          }
        }
        repeat("a")(3)
        """
      )}
    }
    
    "step/objects" should "evaluate JavaScripty objects according to the lab spec" in {
      assertResult(N(7.0)) { iterateStep(
        """
        const pair = function(x: number, y: number) {
          return {x: x, y: y}      
        };
        const p = pair(3, 4);
        p.x + p.y
        """
      )}
      assertResult(N(5.0)) { iterateStep(
        """
        const suspension = function(f: (a: number) => number, n: number) {
          return {f: f, n: n}      
        };
        const s = suspension((a: number) => a + 1, 4);
        s.f(s.n)
        """
      )} 
    }
  }

}

class Lab4JsyTests extends jsy.tester.JavascriptyTester(None, "lab4", Lab4)
