package com.graham.tictactoe.test

import org.scalatest.FunSuite
import com.graham.tictactoe.Game
 
class TicTacToeTest extends FunSuite {

  test("start") {
  	expect(true) {new Game().start("Y")}
  	expect(true) {new Game().start("y")}
  	expect(true) {new Game().start("yes")}
  	expect(true) {new Game().start("anything")}
  	expect(false) {new Game().start("N")}
  	expect(false) {new Game().start("no")}
  	expect(false) {new Game().start("whatever")}
  }

  test("newBoard") {
  	val results = new Game().newBoard()
  	expect(9) {results.length}
  	expect(9) {results.count(_ == " ")}
  }

  test("get players with player 1 first") {
  	val player1 = "bob"
  	val player2 = "ann"
  	val random = 0
  	expect((("X", "bob"), ("O", "ann"))) {new Game().getPlayers(player1, player2, random)}
  }

  test("get players with player 2 first") {
  	val player1 = "bob"
  	val player2 = "ann"
  	val random = 1
  	expect((("O", "ann"), ("X", "bob"))) {new Game().getPlayers(player1, player2, random)}
  }

  test("coin flip") {
  	val coinFlip = (new Game().coinFlip) 
  	def zeroOrOne (testVal:Int):Boolean = {testVal == 0 || testVal == 1}
  	expect(zeroOrOne(coinFlip)) {true} 
  }

  test("change current player") {
  	val players = (("X", "bob"),("O", "ann"))
  	val currentPlayer = ("X", "bob")
  	expect(("O", "ann")){new Game().changeCurrentPlayer(currentPlayer, players)}
  }

  test("make play") {
  	val board = (Array(" ", " ", " ", " ", " ", " ", " ", " ", " "))
  	val space = 5
  	val player = ("X", "bob")
  	val newBoard = new Game().makePlay(board, space, player)
  	expect("X"){newBoard(4)}
  	expect(" "){newBoard(5)}
  	expect(8){newBoard.count(_ == " ")}
  }

  test("valid space") {
  	val board = Array("O", "X", "X", " ", " ", " ", " ", " ", " ")
  	expect(true){new Game().validSpace(board, "4")}
  	expect(false){new Game().validSpace(board, "1")}
  	expect(false){new Game().validSpace(board, "0")}
  	expect(false){new Game().validSpace(board, "11")}
  	expect(false){new Game().validSpace(board, "a")}
  	expect(false){new Game().validSpace(board, "a4")}
  	expect(false){new Game().validSpace(board, "4a")}
  }

  test("draw") {
    expect(false) { new Game().draw(Array(" ", " ", " ", " ", " ", " ", " ", " ", " ")) }
    expect(false) { new Game().draw(Array("O", "X", "X", " ", " ", " ", " ", " ", " ")) }
    expect(true) { new Game().draw(Array("O", "X", "X", "O", "O", "X", "X", "O", "X")) }
    expect(true) { new Game().draw(Array("X", "O", "O", "O", "X", "X", "O", "X", "O")) }
  }

  test("win") {
  	expect(false) {new Game().win(Array(" ", " ", " ", " ", " ", " ", " ", " ", " "))}
  	expect(true) {new Game().win(Array("O", " ", " ", " ", "O", " ", " ", " ", "O"))}
  	expect(true) {new Game().win(Array("O", "X", "X", "O", "O", "X", "X", "O", "X"))}
  	expect(false) {new Game().win(Array("X", "O", "O", "O", "X", "X", "O", "X", "O"))}
  }
 
}
