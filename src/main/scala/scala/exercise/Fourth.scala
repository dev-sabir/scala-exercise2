package scala.exercise

class MyList(list:List[Int]) {

  def map(f: Int => Int): List[Int] = {
     list.foldLeft(List[Int]()){(acc, elem) => {
       acc :+ f(elem)
     }}
  }


  def flatMap(f: Int => List[Int]): List[Int] = {
     val x = list.map(f)
      x.foldLeft(List[Int]()){(acc,curr) => {
        acc ++ curr
      }}
    }


}

object Fourth {

  def main(args: Array[String]): Unit ={

    val x = new MyList(List(1,2,3))
    println(x.flatMap(i => List(i, 2*i, 3*i)))

    println(x.map(i => i*3))

  }
}
