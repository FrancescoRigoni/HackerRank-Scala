import java.io._

// https://www.hackerrank.com/challenges/the-grid-search/problem

object SearchGrid {

  def allIndexOf(in: String,
                 of: String): Array[Int] = {

    var index = in indexOf of
    var indexes = Array[Int]()
    while (index != -1) {
      indexes = indexes :+ index
      index = in indexOf(of, index + 1)
    }
    indexes
  }

  def matchLineFromPInG(G: Array[String],
                        P: Array[String],
                        lineInG: Int,
                        lineInP: Int): Array[Array[Int]] =

    if (lineInP == P.length)
      Array()
    else if (lineInG == G.length)
      Array()
    else {
      val indexesOfMatch = allIndexOf(G(lineInG), P(lineInP))
      if (indexesOfMatch.nonEmpty) {
        indexesOfMatch +: matchLineFromPInG(G, P, lineInG + 1, lineInP + 1)
      } else {
        Array()
      }
    }

  // Complete the gridSearch function below.
  def gridSearch(G: Array[String], P: Array[String]): String = {
    for (rowInG <- G.indices) {
      val matchedPotitionsInLines = matchLineFromPInG(G, P, rowInG, 0)
      if (matchedPotitionsInLines.nonEmpty) {
        val first = matchedPotitionsInLines(0)
        val intersection = matchedPotitionsInLines.drop(1).foldLeft(first) {
          (acc, item) => acc.intersect(item)
        }
        if (intersection.nonEmpty && matchedPotitionsInLines.length == P.length) return "YES"
      }
    }

    "NO"
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val t = stdin.readLine.trim.toInt

    for (tItr <- 1 to t) {
      val RC = stdin.readLine.split(" ")

      val R = RC(0).trim.toInt

      val C = RC(1).trim.toInt

      val G = Array.ofDim[String](R)

      for (i <- 0 until R) {
        val GItem = stdin.readLine
        G(i) = GItem
      }

      val rc = stdin.readLine.split(" ")

      val r = rc(0).trim.toInt

      val c = rc(1).trim.toInt

      val P = Array.ofDim[String](r)

      for (i <- 0 until r) {
        val PItem = stdin.readLine
        P(i) = PItem
      }

      val result = gridSearch(G, P)

      printWriter.println(result)
    }

    printWriter.close()
  }
}


