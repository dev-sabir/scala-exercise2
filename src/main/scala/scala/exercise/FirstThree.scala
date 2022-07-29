package scala.exercise

import scala.annotation.tailrec

sealed trait Expr
case class Number(i: Int) extends Expr
case class Sum(expr1: Expr, expr2: Expr) extends Expr
case class Subtract(expr1: Expr, expr2: Expr) extends Expr
case class Multiply(expr1: Expr, expr2: Expr) extends Expr

class FirstThree {

  // Problem 1
  // Write a program that generate the word count and Sort value by descending order and key in accenting order
  def wordCount(str: String):List[(String,Int)] = {
    val wordList = str.split(" ")
    val x = wordList.groupBy(a => a)
    val res = x.map(a => (a._1 -> a._2.size))
    println(res)
    res.toList.sortWith((a,b) => a._2 > b._2 || (a._2 == b._2 && a._1 < b._1))
  }

  // Problem 2.1
  // Reverse the List Using foldLeft Method

  def reverseUsingFoldLeft(list: List[Int]): List[Int] ={
    list.foldLeft(List[Int]()){(acc, elem) => elem +: acc}
  }

  // Problem 2.2
  // Reverse the List Using Recursion
  def reverseUsingRecursion(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case _ => reverseUsingRecursion(list.tail) :+ list.head
    }
  }

  // Problem 2.3
  // Reverse List Using Tail Recursion

  def reverseUsingTailRecursion(list: List[Int]) : List[Int] = {
    @tailrec
    def helper(l: List[Int], revList: List[Int]): List[Int] ={
      l match {
        case Nil => revList
        case head :: tail => helper(tail,  head +: revList)
      }
    }
     helper(list, List())
  }

  //Problem 3
  def eval(expr: Expr): Int = {
    expr match {
      case Sum(expr1, expr2) => eval(expr1) + eval(expr2)

      case Subtract(expr1, expr2) => eval(expr1) - eval(expr2)

      case Multiply(expr1, expr2) =>  eval(expr1) * eval(expr2)

      case Number(i) => i
    }
  }

}

object OneToThree{

  def main(args: Array[String]): Unit ={
      val cls = new FirstThree
      // Problem 1
      println("\n###Problem 1 Result###")
      val res = cls.wordCount(("Hi Hi Hello Hi Happy Hello Hi World Hello Hello Hello Happy Scala Hi Happy Happy Happy R "))
      println(res)

      // Problem 2
      println("\n###Problem 2 Result###")
      println(cls.reverseUsingFoldLeft(List(1,2,3,4,5)))
      println(cls.reverseUsingRecursion(List(1,2,3,4)))
      println(cls.reverseUsingTailRecursion(List(10,20,30,40,50)))


      // Problem 3
      println("\n###Problem 3 Result###")
      val expr1 = Sum(Number(10),Number(5))
      println(cls.eval(expr1))

      val expr2 = Subtract(Number(8), Number(7))
      println(cls.eval(expr2))

      val expr3 = Multiply(Number(10), Number(10))
      println(cls.eval(expr3))



  }
}
