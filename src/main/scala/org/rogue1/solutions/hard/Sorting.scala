package org.rogue1.solutions.hard

import scala.annotation.tailrec

object Sorting {

  /**
    *
    * @param input
    * @return
    */
  def selectionSort(input: Array[Int]): Array[Int] = {
    for { i <- input.indices.reverse ; j <- 0 until i }
      if (input(j) > input(j+1)) { val temp = input(j) ; input(j) = input(j+1); input(j+1) = temp }
    input
  }

  /**
    *
    * @param input
    * @return
    */
  def mergeSort(input: List[Int]): List[Int] = {
    def merge(input1: List[Int], input2: List[Int]): List[Int] = {
      (input1, input2) match {
        case (Nil, x) => x
        case (x, Nil) => x
        case (h1 :: tail1, h2 :: tail2) if h1 < h2 => h1 :: merge(tail1, h2 :: tail2)
        case (h1 :: tail1, h2 :: tail2) if h1 > h2 => h2 :: merge(tail2, h1 :: tail1)
      }
    }
    input match {
      case x1 :: x2 :: Nil => if (x1 < x2) x1 :: x2 :: Nil else x2 :: x1 :: Nil
      case x => val (l1, l2) = x.splitAt(x.length/2); merge(mergeSort(l1), mergeSort(l2))
    }
  }




}
