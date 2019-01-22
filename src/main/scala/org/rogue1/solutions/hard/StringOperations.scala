package org.rogue1.solutions.hard

class StringOperations {

  /**
    * Given an integer n, find the closest integer (not including itself), which is a palindrome.
    *
    * The 'closest' is defined as absolute difference minimized between two integers.
    *
    * Example 1:
    * Input: "123"
    * Output: "121"
    * Note:
    * The input n is a positive integer represented by string, whose length will not exceed 18.
    * If there is a tie, return the smaller one as answer.
    *
    * @param n
    * @return
    */
  def nearestPalindromic(n: String): String = {
    def isPalimdrome(int: Int): Boolean = int.abs.toString.reverse == int.abs.toString
    val num = n.toInt
    val int = Stream.from(0,1)
      .map(x => ((num + x) -> isPalimdrome(num + x)) -> ((num - x) -> isPalimdrome(num - x)))
      .dropWhile({ case ((_,x), (_, y)) => !x && !y })
      .head match {
      case ((x, true), _) => x
      case ((_, false), (x, true)) => x
      case _ => ???
    }
    int.toString
  }

  /**
    * Given an input string (s) and a pattern (p), implement wildcard pattern matching with support for '?' and '*'.
    *
    * '?' Matches any single character.
    * '*' Matches any sequence of characters (including the empty sequence).
    * The matching should cover the entire input string (not partial).
    *
    * Note:
    *
    * s could be empty and contains only lowercase letters a-z.
    * p could be empty and contains only lowercase letters a-z, and characters like ? or *.
    * Example 1:
    *
    * Input:
    * s = "aa"
    * p = "a"
    * Output: false
    * Explanation: "a" does not match the entire string "aa".
    * Example 2:
    *
    * Input:
    * s = "aa"
    * p = "*"
    * Output: true
    * Explanation: '*' matches any sequence.
    * Example 3:
    *
    * Input:
    * s = "cb"
    * p = "?a"
    * Output: false
    * Explanation: '?' matches 'c', but the second letter is 'a', which does not match 'b'.
    *
    * @param str
    * @param pattern
    * @return
    */
  def isMatch(str: String, pattern: String): Boolean = {
    def advanceThroughWord(word: String): List[String] = word.indices.map(x => word.substring(x)).toList
    str.toList -> pattern.toList match {
      case (Nil, Nil) => true
      case (_ :: sTail, '?' :: pTail) => isMatch(sTail.mkString(""), pTail.mkString(""))
      case (_ :: _, '*' :: Nil) => true
      case (_ :: sTail, '*' :: pTail) => advanceThroughWord(sTail.mkString("")).dropWhile(!isMatch(_, pTail.mkString(""))).nonEmpty
      case (x :: sTail, y :: pTail) if x == y => isMatch(sTail.mkString(""), pTail.mkString(""))
      case (x :: _, y :: _) if x != y => false
      case (_, _) => false
    }
  }


  /**
    * Given an array of words and a width maxWidth, format the text such that each line has exactly maxWidth
    * characters and is fully (left and right) justified.
    * You should pack your words in a greedy approach; that is, pack as many words as you can in each line.
    * Pad extra spaces ' ' when necessary so that each line has exactly maxWidth characters.
    * Extra spaces between words should be distributed as evenly as possible. If the number of spaces on a
    * line do not divide evenly between words, the empty slots on the left will be assigned more spaces than the slots on the right.
    * For the last line of text, it should be left justified and no extra space is inserted between words.
    *
    * Note:
    *
    * A word is defined as a character sequence consisting of non-space characters only.
    * Each word's length is guaranteed to be greater than 0 and not exceed maxWidth.
    * The input array words contains at least one word.
    * Example 1:
    *
    * Input:
    * words = ["This", "is", "an", "example", "of", "text", "justification."]
    * maxWidth = 16
    * Output:
    * [
    * "This    is    an",
    * "example  of text",
    * "justification.  "
    * ]
    *
    * @param words
    * @param maxWidth
    * @return
    */
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {
    def justify(input: List[String], maxWidth: Int): String = {
      val wordSpacing = " " * (((maxWidth - (input.map(_.length + 1).sum - 1))/input.length)+1)
      val linePadding = " " * ((maxWidth - (input.map(_.length + 1).sum - 1)) % input.length)
      s"${input.mkString(wordSpacing)}$linePadding"
    }
    val data = words.tail.foldLeft(List[List[String]](List(words.head)))({
      case (init :+ last, item) =>
        val list = last :+ item
        if (list.map(_.length + 1).sum - 1 > maxWidth) { init :+ last :+ List(item) }  else init :+list
    })
    data.map(justify(_, maxWidth))
  }


  /**
    * Given two words (beginWord and endWord), and a dictionary's word list, find all shortest transformation sequence(s) from beginWord to endWord, such that:
    *
    * Only one letter can be changed at a time
    * Each transformed word must exist in the word list. Note that beginWord is not a transformed word.
    * Note:
    *
    * Return an empty list if there is no such transformation sequence.
    * All words have the same length.
    * All words contain only lowercase alphabetic characters.
    * You may assume no duplicates in the word list.
    * You may assume beginWord and endWord are non-empty and are not the same.
    * Example 1:
    *
    * Input:
    * beginWord = "hit",
    * endWord = "cog",
    * wordList = ["hot","dot","dog","lot","log","cog"]
    *
    * Output:
    * [
    * ["hit","hot","dot","dog","cog"],
    * ["hit","hot","lot","log","cog"]
    * ]
    *
    * @param beginWord
    * @param endWord
    * @param wordList
    * @return
    */
  def findLadders(beginWord: String,
                  endWord: String,
                  wordList: List[String]): List[List[String]] = {

    def nextWords(word: String, dict: List[String]): List[String] = dict.filter(x => isNextWord(x, word))

    def isNextWord(word: String, refWord: String): Boolean =
      word.zip(refWord).map({case (x,y) => if (x == y) 0 else 1}).sum == 1

    def process(input: List[List[String]], wordList: List[String], endWord: String): List[List[String]] = {
      val result = for {
        list <- input.filterNot(x => x.last == endWord) if list.nonEmpty
        refList: List[String] = wordList.filterNot(list.contains)
        data <- nextWords(list.last, refList).map(list :+ _) if nextWords(list.last, refList).nonEmpty
      } yield data
      result match {
        case Nil => input.collect({ case x @ _ :+ end if end == endWord => x })
        case _ => process(input.collect({ case x @ _ :+ end if end == endWord => x }) ++ result, wordList, endWord)
      }
    }

    val result = process(List(beginWord :: Nil), wordList, endWord)
    result.map(x => x -> x.length).sortBy({ case (_,y) => y}).headOption match {
      case Some((_,y)) => result.filter(_.length == y)
      case None => Nil
    }
  }




}
