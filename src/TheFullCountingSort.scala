import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

import javax.print.DocFlavor.BYTE_ARRAY

import scala.collection.mutable

// https://www.hackerrank.com/challenges/countingsort4/problem

object TheFullCountingSort {

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt
    var frequencies = Array.fill[Int](100)(0)
    var items = Array[(Int, String)]()

    for (nItr <- 1 to n) {
      val xs = stdin.readLine.split(" ")
      val x = xs(0).trim.toInt
      val s = if (nItr <= n/2) "-" else xs(1)

      items = items :+ (x, s)
      frequencies(x) += 1
    }
    frequencies = frequencies.scanLeft(0) {(acc, thisItem) => acc + thisItem}.tail
    val strings = mutable.HashMap[Int, String]()

    for (i <- (0 until n).reverse) {
      val position = frequencies(items(i)._1)
      frequencies(items(i)._1) = frequencies(items(i)._1) - 1
      strings(position) = items(i)._2
    }

    for (i <- 0 to n) {
      if (strings.contains(i)) print(s"${strings(i)} ")
    }
  }
}