package scala.exercise

import scala.annotation.tailrec

class FiveToTen {

  // Problem 5 Compose f and g function
  def composeFunction[A,B,C](g:B=>C, f:A=>B):A=>C ={
    a => g(f(a))
  }

  // Problem 6
  // Returning two Option value in a tuple and returning in Option[(val1, val2)]
  def fuse[A,B](a: Option[A], b: Option[B]) : Option[(A,B)] = {
    Some((a.get, b.get))
  }

  // Problem 7
  //

  def window(length: Int, list: List[Int]): List[List[Int]] ={
    //Used var in this
    //        var resL:List[List[Int]] = Nil
//        for(i <- 0 to ((list.size-length))){
//          var L = List[Int]()
//          for(j <- i until length+i){
//            L = L :+ list(j)
//          }
//          resL = resL :+ L
//
//        }
//    resL

    // Without using var
    @tailrec
    def helper(list1: List[Int], resList: List[List[Int]]): List[List[Int]] ={
      list1.size >= length match {
        case true => helper(list1.drop(1), (resList :+ list1.take(length)))
        case false => resList
      }
    }
    helper(list, List[List[Int]]())
  }

  // Problem 8
  // Zip 2 list using own zip method

  def myZip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    val starter: List[(Int, String)] = Nil
    if(list1.size > list2.size){
      List.range(0,list2.size).foldLeft(starter){(acc, counter) => {
        acc :+ (list1(counter), list2(counter))
      }}
    }else{
      List.range(0, list1.size).foldLeft(starter){(acc, counter) => {
        acc :+ (list1(counter), list2(counter))
      }}
    }
  }

  // Problem 9
  //
  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
    val starter: Map[String, Int] = Map()
//    val starter = Map():Map[String, Int]
    list.foldLeft(starter){(acc, elem) => {
      acc ++ elem
    }}
  }

  def reduce2(list: List[Map[String, Map[String, Int]]]) : Map[String, Map[String, Int]] = {
    val starter = Map():Map[String, Map[String, Int]]
    list.foldLeft(starter){(acc, elem) => {
      acc ++ elem
    }}
  }


  // Problem 10
  //merge
  def merge(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    val starter = map2
    map1.foldLeft(starter){(acc, elem) => {
      map2.contains(elem._1) match {
        case true => acc + (elem._1 -> (elem._2+ map2(elem._1)))
        case false => acc + elem
      }
    }}
  }

  // merge2
  def merge2(map1: Map[String,Map[String,Int]], map2: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
    map1.foldLeft(map2){(acc, elem) => {
      map2.contains(elem._1) match {
        case true => (acc + (elem._1 -> merge(elem._2,map2(elem._1) ))) //Calling the above merge method
        case false => acc + elem
      }
    }}
  }

  // merge 3

  def merge3(map1: Map[String, Map[String, Map[String, Int]]], map2: Map[String, Map[String, Map[String, Int]]]): Map[String,Map[String, Map[String, Int]]] = {
    map1.foldLeft(map2){(acc, elem) => {
      map2.contains(elem._1) match {
        case true => (acc + (elem._1 -> merge2(elem._2, map2(elem._1))))  //Calling Above merge2 method
        case false => acc + elem
      }
    }}
  }


}

object AssignFiveToTen{

  def main(args: Array[String]): Unit = {

    val cls = new FiveToTen

    println("\n###Problem 6 Result###")
    println(cls.fuse(Some(10.00), Some("Hello")))

    println("\n###Problem 7 Result###")
    println(cls.window(3, List(1,2,3,4,5)))

    println("\n###Problem 8 Result###")
    println(cls.myZip(List(1,2,3,4), List("a", "b", "c","d", "e")))

    println("\n###Problem 9 Result###")
    println(cls.reduce(List(Map("hii" -> 1, "Hello"->2), Map("a"->3, "b"->4, "c"->5))))
    val l = List(Map("Hello" -> Map("Hii" -> 2)), Map("Wlcm" -> Map("Scala" -> 2)))
    println(cls.reduce2(l))

    println("\n###Problem 10 Result###")
    // Merge two map into one
    println(cls.merge(Map("hello" -> 1 , "hi" -> 2), Map("hi" -> 2, "you" -> 1)))

    // merge2
    val m1 = Map("Hello" -> Map("Hii" -> 2), "Wlcm" -> Map("Scala" -> 2))
    val m2 = Map("H" -> Map("Hi" -> 2), "Hello" -> Map("Hii" -> 2))
    println(cls.merge2(m1, m2))

    // merge3
    val m3 = Map("A" -> Map("a" -> Map("i" -> 1)), "B" -> Map("b" -> Map("ii" ->2)), "C" -> Map("c"-> Map("iii" -> 3)))
    val m4 = Map("D" -> Map("d" -> Map("iv" -> 4)), "B" -> Map("b" -> Map("ii" -> 5)), "A" -> Map("y" -> Map("vv" -> 55)))
    println(cls.merge3(m3, m4))


  }

}
