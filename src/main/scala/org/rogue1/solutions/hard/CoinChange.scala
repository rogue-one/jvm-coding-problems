package org.rogue1.solutions.hard

object CoinChange {

  /**
    * You are given coins of different denominations and a total amount of money.
    * Write a function to compute the number of combinations that make up that amount.
    * You may assume that you have infinite number of each kind of coin.
    *
    * Input: amount = 5, coins = [1, 2, 5]
    * Output: 4
    * Explanation: there are four ways to make up the amount:
    * 5=5
    * 5=2+2+1
    * 5=2+1+1+1
    * 5=1+1+1+1+1
    *
    */
  def change(amount: Int, coins: Array[Int]): Int = {
    val data = coins.toList.flatMap(x => List.fill(amount/x)(x))
    val result = for {
      i <- 1 to data.length
      list <- data.combinations(i) if list.sum == amount
    } yield list
    result.length
  }

  /**
    * You are given coins of different denominations and a total amount of money amount.
    * Write a function to compute the fewest number of coins that you need to make up that amount.
    * If that amount of money cannot be made up by any combination of the coins, return -1.
    *
    * Input: coins = [1, 2, 5], amount = 11
    * Output: 3
    * Explanation: 11 = 5 + 5 + 1
    *
    * @param coins
    * @param amount
    * @return
    */
  def fewestChange(coins: Array[Int], amount: Int): Int = {
    val data = coins.toList.flatMap(x => List.fill(amount/x)(x))
    val result = for {
      i <- 1 to data.length
      list <- data.combinations(i) if list.sum == amount
    } yield list
    println(result.mkString(","))
    result.map(_.length).sorted.headOption match {
      case Some(x) => x
      case None => -1
    }
  }

  /**
    * Given an array nums, we call (i, j) an important reverse pair if i < j and nums[i] > 2*nums[j].
    *
    * You need to return the number of important reverse pairs in the given array.
    *
    * Example1:
    *
    * Input: [1,3,2,3,1]
    * Output: 2
    * Example2:
    *
    * Input: [2,4,3,5,1]
    * Output: 3
    * Note:
    * The length of the given array will not exceed 50,000.
    * All the numbers in the input array are in the range of 32-bit integer.
    *
    * @param nums
    * @return
    */
  def reversePairs(nums: Array[Int]): Int = {
    val data = for {
      i <- nums.indices
      j <- i until nums.length if nums(i) > 2 * nums(j)
    } yield (i,j)
    data.length
  }

}
