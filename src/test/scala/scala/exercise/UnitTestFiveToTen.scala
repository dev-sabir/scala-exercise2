package scala.exercise

import org.scalatest.funsuite.AnyFunSuite

class UnitTestFiveToTen extends AnyFunSuite{
    // Problem 6 Test Case
    val cls = new FiveToTen
    test("fuse function testing"){
      val res1 = cls.fuse(Some(1), Some("Hii"))
      val res2 = cls.fuse(Some("H"), Some("H"))
      val res3 = cls.fuse(Some(None), Some(None))
      assert(res1 === Some(1, "Hii") && res2 === Some("H", "H") && res3 === Some(None, None))
    }

   // Problem 7 Test Case
    test("window method testing"){
      val res1 = cls.window(2, List(1,2,3,4,5))
      val res2 = cls.window(3, List(1,2,3,4,5))
      val res3 = cls.window(4, List(1,2,3,4,5))
      val res4 = cls.window(5, List(1,2,3,4,5))
      val res5 = cls.window(6, List(1,2,3,4,5))
      assert(res1 === List(List(1,2), List(2,3), List(3,4), List(4,5)) &&
             res2 === List(List(1,2,3), List(2,3,4), List(3,4,5)) &&
             res3 === List(List(1,2,3,4), List(2,3,4,5)) &&
             res4 === List(List(1,2,3,4,5)) &&
             res5 === List()
            )
    }

    // Problem 8
    test("zip 2 list method testing"){
      val res1 = cls.myZip(List(1,2,3), List("a", "b"))
      assert(res1 == List((1,"a"), (2,"b")))
      val res2 = cls.myZip(List(1,2,3), List("a", "b", "c"))
      assert(res2 === List((1,"a"), (2, "b"), (3, "c")))
      val res3 = cls.myZip(List(1,2), List("a", "b", "c"))
      assert(res3 === List((1,"a"), (2,"b")))
    }

    // Problem 9
    test("testing for problem 9"){
      val res1 = cls.reduce(List(Map("A"->1, "B"->2), Map("C"->3, "D"->4)))
      assert(res1 === Map("A"->1, "B"->2, "C"->3, "D"->4))

      val res2 = cls.reduce2(List(Map("A"-> Map("a"->1)), Map("B"-> Map("b"->2))))
      assert(res2 === Map("A" -> Map("a"->1), "B"-> Map("b"->2)))
    }

    // Problem 10
    test("merging of two map into one"){
      val res1 = cls.merge(Map("a"->1, "b"->2), Map("c"->2, "b"->4))
      assert(res1 === Map("a"->1,"b"->6, "c"->2))

      // merge2
      val m1 = Map("Hello" -> Map("Hii" -> 2), "Wlcm" -> Map("Scala" -> 2))
      val m2 = Map("H" -> Map("Hi" -> 2), "Hello" -> Map("Hii" -> 2))
      val res2 = cls.merge2(m1,m2)
      val expt = Map("H" -> Map("Hi" -> 2), "Hello" -> Map("Hii" -> 4), "Wlcm" -> Map("Scala" -> 2))
      assert(res2 === expt)

      // merge3
      val m3 = Map("A" -> Map("a" -> Map("i" -> 1)), "B" -> Map("b" -> Map("ii" ->2)), "C" -> Map("c"-> Map("iii" -> 3)))
      val m4 = Map("D" -> Map("d" -> Map("iv" -> 4)), "B" -> Map("b" -> Map("ii" -> 5)), "A" -> Map("y" -> Map("vv" -> 50)))
      val res3 = cls.merge3(m3,m4)
      val expt1 = Map("D" -> Map("d" -> Map("iv" -> 4)), "B" -> Map("b" -> Map("ii" -> 7)), "A" -> Map("y" -> Map("vv" -> 50), "a" -> Map("i" -> 1)), "C" -> Map("c" -> Map("iii" -> 3)))

      assert(res3 === expt1)
    }

}
