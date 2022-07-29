package scala.exercise

import org.scalatest.funsuite.AnyFunSuite

class UnitTestFourth extends AnyFunSuite{
    //Problem 4
    val cls = new MyList(List(1,2,3,4))
    test("Implementation of map"){
      val res1 = cls.map(a => a*a)
      assert(res1 === List(1,4,9,16))
    }

    test("Implementation of flatMap"){
      val res1 = cls.flatMap(a => List(a+1, a+2,a+3,a+4))
      assert(res1 === List(2,3,4,5,3,4,5,6,4,5,6,7,5,6,7,8))
    }

}
