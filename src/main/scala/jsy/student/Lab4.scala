package jsy.student

import jsy.lab4.Lab4Like

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
   * Isaiah
   * 
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /*** Collections and Higher-Order Functions ***/
  
  /* Lists */
  // CONSECUTIVE dupes, not all dupes
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => 
      if(h1 != h2) h1 :: compressRec(t1)
      else compressRec(t1)
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => if (acc == Nil || acc.head != h) h :: acc
      else acc
  }
  
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => 
      val acc = f(h)
      acc match {
        case Some(x) => x :: t
        case None => h :: mapFirst(t)(f)
      }
      
  }
  
  /* Trees */

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc
      case Node(l, d, r) => loop(  loop(  f(acc,d)  , l )  ,r)
        
      //f( foldLeft(l)(z)(f), d ); f( foldLeft(r)(z)(f), d ) 
      //foldLeft(l)(f(z,d))(f); foldLeft(r)(f(z,d))(f)
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) =>  acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = foldLeft(t)((true, None: Option[Int])){
      //acc is (bool, Opt[int]), s is an int
       (acc, s) => acc match {
        case (c, None) =>  (true && c, Some(s))
        case (c,Some(x)) => 
          //println("s = " + s);
          //println("x = " + x);
          //println("acc: " + acc);
          if(s > x)  (true && c, Some(s))
          else (false && c, Some(s))

        
       }
        //got this one quickly but lost so much time because my foldLeft was erreneous 
      
    }
    //println("t: " + t)
    //println("b: " + b)
    b
  }

  /*** Rename bound variables in e ***/

  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env: Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1)) //Print(ren(e1))

        case Unary(uop, e1) => Unary(uop, ren(env,e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(env,e1), ren(env,e2))
        case If(e1, e2, e3) => If(ren(env,e1), ren(env,e2), ren(env,e3))

        case Var(y) => 
          val vg = env.get(y)
          val p = vg match {
            case None => y
            case Some(s) => s
          }
          Var(p)
        case Decl(mode, y, e1, e2) =>
          val yp = fresh(y)
          println(y); println(yp)
          Decl(mode, yp, ren(env,e1), ren(env + (y -> yp), e2))
          

        case Function(p, params, tann, e1) => {
          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => (None, env)
            case Some(x) => 
              val xp = fresh(x)
              (Some(xp), env + (x -> xp))
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) {
            ???
          }
          ???
        }

        case Call(e1, args) => Call(ren(env,e1), args)

        case Obj(fields) => Obj(fields map { case (fn,expn) => (fn, ren(env,expn)) })
        case GetField(e1, f) => GetField(ren(env,e1), f)
      }
    }
    ren(empty, e)
  }

  /*** Type Inference ***/

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
  
  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x) /*env.get(x) match {
        case None => ???  //error
        case Some(y) => ??? 
      } */
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TString, TString) => TString
        
        case (TNumber, TNumber) => TNumber
        
        case (tgot, tgot2) => err(tgot, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) =>  (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (tgot, tgot2) => err(tgot, e1)
      }
      case Binary(Eq|Ne, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (tg, tg2) if( !hasFunctionTyp(tg) && !hasFunctionTyp(tg2)) => TBool
        case (tg,_) => err(tg,e1)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TString, TString) => TBool
        
        case (TNumber, TNumber) => TBool
        
        case (tgot, tg2) => err(tgot, e1)
      }
      case Binary(And|Or, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TBool, TBool) => TBool
        case (tgot, tg2) => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) => typeof(env, e2)
      case If(e1, e2, e3) => (typeof(env, e1), typeof(env, e2), typeof(env, e3)) match {
        case (TBool, tg1, tg2) => if (tg1 == tg2) tg1 else err(tg1, e1)
        case (tgot, _, _) => err(tgot, e1)
      }
      case Obj(fields) => TObj(fields map { case (f,t) => (f, typeof(env,t)) })
      case GetField(e1, f) => typeof(env,e1) match {
        case TObj(ta) if (ta.contains(f)) => ta(f)
        case tgot => err(tgot, e1)
      }

      case Decl(m, x, e1, e2) => 
        TUndefined

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          /***** Add cases here *****/
          case (Some(x), Some(t)) =>  
            val tp = TFunction(params, t)
            extend(env, x, tp)
            // TypeRecFunction
          
            case(None, _) => env 

          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1){ (envacc,parami) =>
          val (xi, MTyp(_, ti)) = parami
          extend(envacc, xi, ti)

        }
        // Infer the type of the function body
        val t1 = typeof(env2,e1)
        // Check with the possibly annotated return type
        val tret = TFunction(params, t1)
        tann match {
          case Some(t) => if(t1 == t) tret else err(t1, e1) 
          case None => tret
        }
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params zip args).foreach { zippedarg =>
            val (parami, ei) = zippedarg
            val (_, MTyp(_, ti)) = parami
            val tgot = typeof(env, ei)
            if( tgot == ti){

            }
            else err(tgot, ei)
            
          }
          tret
        case tgot => err(tgot, e1)
      }

    }
  }
  
  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) => (bop: @unchecked) match {                    // EvalInequalityString and DoInequalityString helper
        case Lt => s1 < s2
        case Le => s1 <= s2
        case Gt => s1 > s2
        case Ge => s1 >= s2
      }
      case (N(n1), N(n2)) => (bop: @unchecked) match {    // EvalInequalityNumber1 and DoInequalityNumber1 helper
        case Lt => n1 < n2
        case Le => n1 <= n2
        case Gt => n1 > n2
        case Ge => n1 >= n2
      }
      
    }
  }

  /* This should be the same code as from Lab 3 */
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e, n) match {
      case None => e
      case Some(ep) => loop(ep, n + 1)
    }
    loop(e0, 0)
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (y == x) esub else e
      case Decl(mode, y, e1, e2) => if (y == x) Decl(mode, y, subst(e1), e2) // needed to respect shadowing. EX: if a = 3, "a; (const a = 4; a)" becomes "3; (const a = 4; a)" instead of the a in e2 getting captured by 3 as well
        else Decl(mode, y, subst(e1), subst(e2))
        /***** Cases needing adapting from Lab 3 */
      case Function(p, params, tann, e1) =>
        Function(p, params, tann, subst(e1))
      case Call(e1, args) => Call(subst(e1), args map subst)
        /***** New cases for Lab 4 */
      case Obj(fields) => Obj(fields map { case (f,expn) => (f, subst(expn)) })
      case GetField(e1, f) => GetField(subst(e1), f)
    }

    val fvs = freeVars(esub)
    def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x

    subst(e) // change this line if/when you implement capture-avoidance for call-by-name
  }

  /* Helper function for implementing call-by-name: check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => ???
    case MName => ???
  }

  /* A small-step transition. */
  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, v1) if isValue(v1) => v1 match {
        case N(n1)=> N(- n1)
      }
        /***** More cases here */
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, _, e1) => {
            val pazip = params zip args
            if (args forall isValue) {
              val e1p = pazip.foldRight(e1) {
                case((p,a),e1) => substitute(e1,a, p._1)
              }
              p match {
                case None => e1p
                case Some(x1) => substitute(e1p, v1, x1)
              }
            }
            else {
              val pazipp = mapFirst(pazip) {
                ???
              }
              ???
            }
          }
          case _ => throw StuckError(e)
        }
        /***** New cases for Lab 4. */

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
        /***** Cases from Lab 3. */
      case Unary(uop, e1) => Unary(uop, step(e1))
        /***** More cases here */
        /***** Cases needing adapting from Lab 3 */
      case Call(v1 @ Function(_, _, _, _), args) => ???
      case Call(e1, args) => ???
        /***** New cases for Lab 4. */

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left are ones that should be stuck.
       */
      case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
