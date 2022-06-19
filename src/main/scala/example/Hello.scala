package example

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import example.Game._
import example.Side.{Heads, Tails}
import org.http4s.server.middleware.HttpMethodOverrider.HeaderOverrideStrategy

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random

case class GameState(flipsCount: Int, correctGuesses: Int)

object Game {
  def showPrompt: Unit = print("\n(h)eads, (t)ails, or (q)uit: ")

  def getUserInput = StdIn.readLine().trim.toLowerCase

  def printableFlipResult(flip: String): String = flip match {
    case "h" => "Heads"
    case "t" => "Tails"
  }

  def printGame(printableResult: String, state: GameState): Unit = {
    println(s"Flip was: ${printableResult}")
    printGameState(state)
  }

  def printGameOver(): Unit = {
    println("Game Over")
  }

  def printGameState(state: GameState): Unit = {
    println(s"Flips : ${state.flipsCount}, correct: ${state.correctGuesses}")
  }

  def tossCoin(r: Random): String = {
    r.nextInt(2) match {
      case 0 => "h"
      case 1 => "t"
    }
  }
}

sealed trait Side

object Side {
  final case object Heads extends Side

  final case object Tails extends Side
}

object GameIO {


  def showPrompt = IO.delay(print("\n(h)eads, (t)ails, or (q)uit: "))

  def getUserInput = IO.delay(StdIn.readLine().trim.toLowerCase)

  def parseUserInput(input: String): IO[Option[Side]] = input match {
    case "h" => IO.pure(Heads.some)
    case "t" => IO.pure(Tails.some)
    case "q" => IO.none[Side]
    case _ => new IllegalArgumentException(s"Unknown input: ${input}").raiseError[IO, Option[Side]]
  }

  def printableFlipResult(flip: Side) = IO.pure(flip match {
    case Heads => "Heads"
    case Tails => "Tails"
  })

  def formatFlipResult(flip: Side) = flip match {
    case Heads => "Heads"
    case Tails => "Tails"
  }

  def printGame(flipResult: Side, state: GameState) = {
    val str = formatFlipResult(flipResult)
    IO.delay(println(s"Flip was: ${str}")) *> printGameState(state)
  }

  def printGameOver() = {
    IO.delay(println("Game Over"))
  }

  def printGameState(state: GameState) = {
    IO.delay(println(s"Flips : ${state.flipsCount}, correct: ${state.correctGuesses}"))
  }

  def calcualteState(tossResult: Side, userGuess: Side, state: GameState) = {
    val newState = if (tossResult == userGuess) {
      state.copy(flipsCount = state.flipsCount + 1, correctGuesses = state.correctGuesses + 1)
    }
    else {
      state.copy(flipsCount = state.flipsCount + 1)
    }
    IO.pure(newState)
  }

  def tossCoin(r: Random) = {
    IO.pure(r.nextInt(2) match {
      case 0 => Heads
      case 1 => Tails
    })
  }
}

trait Console[F[_]] {
  def readStr: F[String]

  def readBigDecimal: F[BigDecimal]

  def putStrLn(str: String): F[Unit]
}

object Console {

  def apply[F[_] : Sync]: Console[F] =
    new Console[F] {

      def readStr: F[String] = Sync[F].delay(StdIn.readLine())

      def readBigDecimal: F[BigDecimal] =
        readStr.flatMap { str =>
          try BigDecimal(str).pure[F]
          catch {
            case _: Throwable => putStrLn(s"$str is not a BigDecimal") *> readBigDecimal
            //            case _: Throwable =>
            //              putStrLn(s"$str is not a BigDecimal")
            //                .flatMap(_ => readBigDecimal)
          }
        }

      def putStrLn(str: String): F[Unit] = Sync[F].delay(println(str))
    }
}


object GameApp extends IOApp {
  var input = ""
  val state: GameState = GameState(0, 0)
  val r = new Random()

  //  @tailrec
  //  def gameLoop(r: Random, state: GameState): Unit = {
  //    showPrompt
  //    input = getUserInput
  //    if (input == "q") {
  //      printGameOver()
  //    }
  //    else {
  //      val tossResult = tossCoin(r)
  //      val newState = if (tossResult == input) {
  //        state.copy(flipsCount = state.flipsCount + 1, correctGuesses = state.correctGuesses + 1)
  //      }
  //      else {
  //        state.copy(flipsCount = state.flipsCount + 1)
  //      }
  //      val printableFlipResultStr = printableFlipResult(tossResult)
  //      printGame(printableFlipResultStr, state)
  //      gameLoop(r, newState)
  //    }
  //  }
  //
  //  gameLoop(r, state)

  override def run(args: List[String]): IO[ExitCode] = {
    def mainLoop(state: GameState): IO[Unit] =
      for {
        _ <- GameIO.showPrompt
        input <- GameIO.getUserInput
//        token <- GameIO.parseUserInput(input)
        token <- GameIO.parseUserInput(input).handleErrorWith(_ => IO.delay(println(s"unknown input: ${input}")).*>(IO.none))
        new_state <- token match {
          case Some(side) =>
            IO.defer(for {
              tossResult <- GameIO.tossCoin(r)
              new_state <- GameIO.calcualteState(tossResult, side, state)
              _ <- GameIO.printGame(side, new_state)
            } yield new_state
            )
          case None => IO.pure(state)
        }
        _ <- input match {
          case "q" => IO.unit
          case _ => mainLoop(new_state)
        }
      } yield ()

    for {
//      _ <- mainLoop(state).handleErrorWith(_ => IO.delay(println(s"unknown input: ${input}")))
      _ <- mainLoop(state)
    } yield ExitCode.Success
  }
}
