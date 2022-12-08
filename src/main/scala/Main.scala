import solutions._
import scala.util.Failure
import scala.util.Success

@main def hello: Unit =
  println("Day 1 Solution -part 1-:  " + Day1.getElveWithMostCalories)
  println("Day 1 Solution -part 2-:  " + Day1.getTotalCaloriesTop3Elves)

  Day2.startGame match
    case Failure(exception) =>
      println("Error while trying Day2 puzzle :: " + exception.getMessage())
    case Success(games) =>
      val (gameForPart1, gameForPart2) = games
      println("Day 2 Solution -part1-  :: " + gameForPart1.player2TotalScore)
      println("Day 2 Solution -part1-  :: " + gameForPart2.player2TotalScore)
