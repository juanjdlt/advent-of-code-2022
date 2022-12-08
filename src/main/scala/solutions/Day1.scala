package solutions

import scala.io.Source
import scala.util.Using
import scala.util.Try
import scala.util.Failure
import scala.util.Success

// --- Day 1: Calorie Counting ---

object Day1:

  def parseInputFile: Try[List[String]] =
    val file = Source.fromFile("src/main/resources/day1-input.txt")
    val elves: List[List[Int]] = List.empty

    Using(file) { source =>
      val lines = source.getLines()
      lines.toList
    }

  end parseInputFile

  def getElvesCalories: List[Int] =
    parseInputFile match
      case Success(values) =>
        @annotation.tailrec
        def loop(
            remainingItems: List[String],
            currentSum: Int,
            acc: List[Int]
        ): List[Int] =
          if (remainingItems.nonEmpty) {
            val item = remainingItems.head
            if (item == "") then loop(remainingItems.tail, 0, acc :+ currentSum)
            else
              loop(
                remainingItems.tail,
                remainingItems.head.toInt + currentSum,
                acc
              )
          } else {
            acc :+ currentSum
          }

        loop(values, 0, List.empty[Int])
      case Failure(exception) =>
        println(
          "Error while parsing the input file :: " + exception.getMessage()
        )
        List.empty
  end getElvesCalories

  def getElveWithMostCalories: Int =
    getElvesCalories.max

  def getTotalCaloriesTop3Elves: Int =
    getElvesCalories.sortWith(_ > _).take(3).sum
