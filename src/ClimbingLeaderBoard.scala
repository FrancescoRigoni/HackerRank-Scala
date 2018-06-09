
object ClimbingLeaderBoard {

  // Complete the climbingLeaderboard function below.
  def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
    var uniqueScores = scores.distinct
    var rankings = Array[Int]()

    alice foreach {
      aliceScore =>
        val partitions = uniqueScores.partition(i => i < aliceScore)
        val secondPartitionPlusAlice = (partitions._2 :+ aliceScore).distinct

        rankings = rankings :+ secondPartitionPlusAlice.length
        uniqueScores = (scores :+ aliceScore).distinct
    }

    rankings
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val scoresCount = stdin.readLine.trim.toInt

    val scores = stdin.readLine.split(" ").map(_.trim.toInt)
    val aliceCount = stdin.readLine.trim.toInt
    val alice = stdin.readLine.split(" ").map(_.trim.toInt)

    val result = climbingLeaderboard(scores, alice)

    println(result.mkString("\n"))
  }
}

