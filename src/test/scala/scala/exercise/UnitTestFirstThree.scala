package scala.exercise

import org.scalatest.funsuite.AnyFunSuite

class UnitTestFirstThree extends AnyFunSuite{


    val cls = new FirstThree
    // Problem 1 Test Case
    test("Word Count and values in descending order and if values are same then then key in ascending"){
      val res1 = cls.wordCount("Hello Hi Hello Hi World Scala")
      println(res1)
      assert(res1 ===  List(("Hello",2),("Hi",2),("Scala",1),("World",1)))
    }

    // Problem 2 Test Case
    test("Reversing of List"){
      val res1 = cls.reverseUsingFoldLeft(List(1,2,3,4,5))
      val res2 = cls.reverseUsingRecursion(List(10,20,30,40,50))
      val res3 = cls.reverseUsingTailRecursion(List(50,51,55,57,60))
      assert(res1 === List(5,4,3,2,1) && res2 === List(50,40,30,20,10) && res3 === List(60,57,55,51,50))

    }

    //Problem 3 Test Case
    test("Implement the eval method which handle any combination of sub class of Expr for"){
      val res1 = cls.eval(Sum(Number(2),Number(10)))
      val res2 = cls.eval(Sum(Number(2), Number(0)))
      assert(res1 === 12 && res2 === 2)
      val res3 = cls.eval(Multiply(Number(10), Number(3)))
      val res4 = cls.eval(Multiply(Number(0), Number(10)))
      assert(res3 === 30 && res4 == 0)
      val res5 = cls.eval(Subtract(Number(10),Number(3)))
      val res6 = cls.eval(Subtract(Number(15), Number(21)))
      assert(res5 === 7 && res6 == -6)
    }

}
