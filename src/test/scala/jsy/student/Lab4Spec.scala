package jsy.student

import jsy.lab4.Lab4Like
import jsy.lab4.ast._
import jsy.lab4.Parser.parse
import jsy.tester.JavascriptyTester
import org.scalatest._
import flatspec._

class Lab4Spec(lab4: Lab4Like) extends AnyFlatSpec {
  import lab4._

  /***** Higher-Function Exercises Tests *****/

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst(l1) { (i: Int) => if (i < 0) Some(-i) else None }
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }

    assertResult(20){
      sum(treeFromList(List(2,4,6,8)))
    }

    assertResult(2){
      sum(treeFromList(List(0,2)))
    }

    assertResult(21){
      sum(treeFromList(List(0,2,3,4,5,7)))
    }

    assertResult(20){
      sum(treeFromList(List(0,2,2,4,5,7)))
    }

    assertResult(17){
      sum(treeFromList(List(0,2,2,4,5,4)))
    }




  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
    assert(!strictlyOrdered(treeFromList(List(1,2,4,3))))
    assert(strictlyOrdered(treeFromList(List(1,2,6,9,10))))
    assert(!strictlyOrdered(treeFromList(List(1,2,6,9,10,10))))
    assert(!strictlyOrdered(treeFromList(List(1,2,3,3))))
    assert(strictlyOrdered(treeFromList(List(1))))
  }


  /***** Interpreter Tests *****/

  {
    val xtype = TNumber
    val tenvx = extend(empty, "x", xtype)

    "TypeVar" should "perform TypeVar" in {
      assertResult(xtype) {
        typeof(tenvx, Var("x"))
      }
    }

    

    // Probably want to write some more tests for typeInfer, substitute, and step.

  }

  "substitute" should "perform syntatic substitution respecting shadowing" in {
    val xplus1 = parse("x + 1")
    val twoplus1 = parse("2 + 1")
    assert(substitute(xplus1, N(2), "x") === twoplus1)
    val constx3 = parse("const x = 3; x")
    val shadowx = Binary(Plus, constx3, Var("x"))
    assert(substitute(shadowx, N(2), "x") === Binary(Plus, constx3, N(2)))
  }

}

// An adapter class to pass in your Lab4 object.
class Lab4SpecRunner extends Lab4Spec(jsy.student.Lab4)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab4JsyTests extends JavascriptyTester(None, "lab4", jsy.student.Lab4)

