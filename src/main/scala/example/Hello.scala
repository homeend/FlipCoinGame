package example

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import example.Side.{Heads, Tails}

import scala.io.StdIn
import scala.util.Random
import scala.util.control.NoStackTrace

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

sealed trait Command

sealed trait Side2 extends Command

object Side2 {
  final object Heads2 extends Side2

  final object Tails2 extends Side2
}

final object Quit extends Command


sealed trait ValidationError extends Throwable with NoStackTrace {
  def getValue() = ""
}

object ValidationError {

  final case class UnknownCommand(str: String) extends ValidationError {
    override def getMessage: String = s"Unknown input: $str"

    override def getValue(): String = str
  }
}


object GameIO {
  def showPrompt = IO.delay(print("\n(h)eads, (t)ails, or (q)uit: "))

  def getUserInput = IO.delay(StdIn.readLine().trim.toLowerCase)


  def parseUserInput(input: String): IO[Either[ValidationError, Command]] = input match {
    case "h" => Right(Side2.Heads2).pure[IO]
    case "t" => Right(Side2.Tails2).pure[IO]
    case "q" => Right(Quit).pure[IO]
    case _ => Left(ValidationError.UnknownCommand(input)).pure[IO]
  }

  def printableFlipResult_(flip: Side) = IO.pure(flip match {
    case Heads => "Heads"
    case Tails => "Tails"
  })

  def printableFlipResult(flip: Side) = flip match {
    case Heads => "Heads".pure[IO]
    case Tails => "Tails".pure[IO]
  }

  def formatFlipResult(flip: Side) = flip match {
    case Heads => "Heads"
    case Tails => "Tails"
  }

  def formatFlipResult2(flip: Side2) = flip match {
    case Side2.Heads2 => "Heads"
    case Side2.Tails2 => "Tails"
  }

  def printGame(flipResult: Side, state: GameState) = {
    val str = formatFlipResult(flipResult)
    IO.delay(println(s"Flip was: ${str}")) *> printGameState(state)
  }

  def printGame2(flipResult: Side2, state: GameState) = {
    val str = formatFlipResult2(flipResult)
    IO.delay(println(s"Flip was: ${str}")) *> printGameState(state)
  }

  def printGameOver() = {
    IO.delay(println("Game Over"))
  }

  def printGameState(state: GameState) = {
    IO.delay(println(s"Flips : ${state.flipsCount}, correct: ${state.correctGuesses}"))
  }

  def calcualteState(tossResult: Side2, userGuess: Side2, state: GameState) = {
    val newState = if (tossResult == userGuess) {
      state.copy(flipsCount = state.flipsCount + 1, correctGuesses = state.correctGuesses + 1)
    }
    else {
      state.copy(flipsCount = state.flipsCount + 1)
    }
    newState.pure[IO]
  }

  def tossCoin(r: Random) = {
    IO.delay(r.nextInt(2) match {
      case 0 => Heads
      case 1 => Tails
    })
  }

  def tossCoin2(r: Random) = {
    IO.delay(r.nextInt(2) match {
      case 0 => Side2.Heads2
      case 1 => Side2.Tails2
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

  def handleError(error: ValidationError): IO[Unit] = IO.delay(println(s"Error: ${error.getValue()}"))

  override def run(args: List[String]): IO[ExitCode] = {
    def mainLoop(state: GameState): IO[Unit] =
      for {
        _ <- GameIO.showPrompt
        input <- GameIO.getUserInput
        token <- GameIO.parseUserInput(input)
        _ <- token.fold(handleError(_).*>(mainLoop(state)), cmd => cmd match {
          case flip: Side2 => for {
            tossResult <- GameIO.tossCoin2(r)
            new_state <- GameIO.calcualteState(tossResult, flip, state)
            _ <- GameIO.printGame2(flip, new_state)
            _ <- mainLoop(new_state)
          } yield IO.unit
          case Quit => IO.unit
        }
        )
      } yield ()

    for {
      _ <- mainLoop(state)
    } yield ExitCode.Success
  }
}
