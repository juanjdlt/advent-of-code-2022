package solutions

import scala.util.Success.apply
import scala.util.Try
import scala.io.Source
import scala.util.Using
import scala.util.Success
import scala.util.Failure

// --- Day 2: Rock Paper Scissors ---

object Day2:

  enum Shape(val points: Int):
    case Rock extends Shape(1)
    case Paper extends Shape(2)
    case Scissors extends Shape(3)

  enum Outcome:
    case Player1Win, Player2Win, Draw

  case class PlayerRound(
      shape: Shape,
      score: Int
  )

  case class Round(
      player1: PlayerRound,
      player2: PlayerRound,
      outcome: Outcome
  )

  case class Game(
      rounds: List[Round],
      player1TotalScore: Int,
      player2TotalScore: Int
  )

  private def parseInputFile: Try[List[(String, String)]] =
    val file = Source.fromFile("src/main/resources/day2-input.txt")

    Using(file) { source =>
      val lines = source.getLines()
      (lines.map { item =>
        val colums = item.split(" ")
        (colums(0).trim -> colums(1).trim)
      }).toList
    }

  end parseInputFile

  private def decryptStrategyGuideForPart1(
      encryptedData: List[(String, String)]
  ): Try[List[(Shape, Shape)]] =

    def decryptPlayer1Value(value: String): Shape =
      value match
        case "A" =>
          Shape.Rock
        case "B" =>
          Shape.Paper
        case "C" =>
          Shape.Scissors
        case _ =>
          throw new Exception(
            "Invalidad encrypted value for PlayerA:" + value
          )
    end decryptPlayer1Value

    def decryptPlayer2Value(value: String): Shape =
      value match
        case "X" => // X: Player2 Rock
          Shape.Rock
        case "Y" => // Y: Player2 Paper
          Shape.Paper
        case "Z" => // Z: Player2 Scissors
          Shape.Scissors
        case _ =>
          throw new Exception(
            "Invalidad encrypted value for PlayerB:" + value
          )

    end decryptPlayer2Value

    Try {
      encryptedData map { encryptedRound =>
        decryptPlayer1Value(encryptedRound._1) -> decryptPlayer2Value(
          encryptedRound._2
        )
      }
    }

  end decryptStrategyGuideForPart1

  private def decryptStrategyGuideForPart2(
      encryptedData: List[(String, String)]
  ): Try[List[(Shape, Shape)]] =
    def decryptPlayer1Value(value: String): Shape =
      value match
        case "A" =>
          Shape.Rock
        case "B" =>
          Shape.Paper
        case "C" =>
          Shape.Scissors
        case _ =>
          throw new Exception(
            "Invalidad encrypted value for PlayerA:" + value
          )
    end decryptPlayer1Value

    def decryptPlayer2Value(value: String, player1Shape: Shape): Shape =
      value match
        case "X" => // Player1 needs to win
          player1Shape match
            case Shape.Rock     => Shape.Scissors
            case Shape.Paper    => Shape.Rock
            case Shape.Scissors => Shape.Paper

        case "Y" => // Draw
          player1Shape
        case "Z" => // Player2 needs to win
          player1Shape match
            case Shape.Rock     => Shape.Paper
            case Shape.Paper    => Shape.Scissors
            case Shape.Scissors => Shape.Rock

        case _ =>
          throw new Exception(
            "Invalidad encrypted value for Player1:" + value
          )

    end decryptPlayer2Value

    Try {
      encryptedData map { encryptedRound =>
        val player1Shape = decryptPlayer1Value(encryptedRound._1)
        val player2Shape = decryptPlayer2Value(encryptedRound._2, player1Shape)

        player1Shape -> player2Shape
      }
    }
  end decryptStrategyGuideForPart2

  private def play(player1Shape: Shape, player2Shape: Shape): Round =
    val (outcomeScorePlayer1, outcomeScorePlayer2, roundOutcome) =
      getScoresAndOutcome(player1Shape, player2Shape)

    Round(
      player1 = PlayerRound(
        shape = player1Shape,
        score = player1Shape.points + outcomeScorePlayer1
      ),
      player2 = PlayerRound(
        shape = player2Shape,
        score = player2Shape.points + outcomeScorePlayer2
      ),
      outcome = roundOutcome
    )

  end play

  private def getScoresAndOutcome(
      playerA: Shape,
      playerB: Shape
  ): (Int, Int, Outcome) =
    (playerA, playerB) match
      case (Shape.Rock, Shape.Scissors)     => (6, 0, Outcome.Player1Win)
      case (Shape.Scissors, Shape.Paper)    => (6, 0, Outcome.Player1Win)
      case (Shape.Paper, Shape.Rock)        => (6, 0, Outcome.Player1Win)
      case (Shape.Rock, Shape.Rock)         => (3, 3, Outcome.Draw)
      case (Shape.Scissors, Shape.Scissors) => (3, 3, Outcome.Draw)
      case (Shape.Paper, Shape.Paper)       => (3, 3, Outcome.Draw)
      case (Shape.Scissors, Shape.Rock)     => (0, 6, Outcome.Player2Win)
      case (Shape.Paper, Shape.Scissors)    => (0, 6, Outcome.Player2Win)
      case (Shape.Rock, Shape.Paper)        => (0, 6, Outcome.Player2Win)

  end getScoresAndOutcome

  def startGame: Try[(Game, Game)] =
    parseInputFile flatMap { encryptedStrategyGuide =>
      decryptStrategyGuideForPart1(
        encryptedStrategyGuide
      ) flatMap { guideForPart1 =>
        decryptStrategyGuideForPart2(
          encryptedStrategyGuide
        ) map { guideForPart2 =>

          val gamePart1 = {
            val rounds = guideForPart1 map { (player1Shape, player2Shape) =>
              play(player1Shape, player2Shape)
            }
            Game(
              rounds = rounds,
              player1TotalScore = rounds.map(_.player1.score).sum,
              player2TotalScore = rounds.map(_.player2.score).sum
            )
          }

          val gamePart2 =
            val rounds = guideForPart2 map { (player1Shape, player2Shape) =>
              play(player1Shape, player2Shape)
            }
            Game(
              rounds = rounds,
              player1TotalScore = rounds.map(_.player1.score).sum,
              player2TotalScore = rounds.map(_.player2.score).sum
            )

          (gamePart1, gamePart2)

        }

      }
    }
  end startGame

end Day2
