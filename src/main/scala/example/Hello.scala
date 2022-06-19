package example

import example.Game._

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

object GameApp extends App {
  var input = ""
  val state: GameState = GameState(0, 0)
  val r = new Random()

  @tailrec
  def gameLoop(r: Random, state: GameState): Unit = {
    showPrompt
    input = getUserInput
    if (input == "q") {
      printGameOver()
    }
    else {
      val tossResult = tossCoin(r)
      val newState = if (tossResult == input) {
        state.copy(flipsCount = state.flipsCount + 1, correctGuesses = state.correctGuesses + 1)
      }
      else {
        state.copy(flipsCount = state.flipsCount + 1)
      }
      val printableFlipResultStr = printableFlipResult(tossResult)
      printGame(printableFlipResultStr, state)
      gameLoop(r, newState)
    }
  }

  gameLoop(r, state)
  //  while (input != "q"){
  //    showPrompt
  //    input = getUserInput
  //    if (input == "q"){
  //      printGameOver()
  //    }
  //    else{
  //      val tossResult = tossCoin(r)
  //      if(tossResult == input) {
  //        state = state.copy(flipsCount = state.flipsCount + 1, correctGuesses = state.correctGuesses + 1)
  //      }
  //      else{
  //        state = state.copy(flipsCount = state.flipsCount + 1)
  //      }
  //      val printableFlipResultStr = printableFlipResult(tossResult)
  //      printGame(printableFlipResultStr, state)
  //    }

}
