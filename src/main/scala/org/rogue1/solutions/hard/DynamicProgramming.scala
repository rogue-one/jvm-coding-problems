package org.rogue1.solutions.hard

object DynamicProgramming {


  def fibonacci(num: Int): Int = {
    val cache = scala.collection.mutable.Map[Int, Int](0 -> 0, 1 -> 1)
    def fib(num: Int): Int = {
      num match {
        case 0 => 0
        case 1 => 1
        case x => cache(x) = fib(x - 2) + fib(x - 1); cache(x)
      }
    }
    fib(num)
  }

}
