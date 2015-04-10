package com.graham.tictactoe
import scala.util.matching.Regex

object TicTacToe {
	def main(args: Array[String]) = {
		new Game().play
	}
}

class Game{
	type Name = String
	type XO = String 

	def play() = {
		while((start(readStartLine())) == true){
			val board = newBoard
			val players = getPlayers(getPlayer1(), getPlayer2(), coinFlip())
			printStart(board, players)
			endGame(board, playGame(board, players))
		}
		println("Goodbye!")
	}

	def start(response: String) :Boolean = {
    		(response.toUpperCase) contains ("Y")
    	}

	def readStartLine(): String = {
	    	readLine("Would you like to play Tic Tac Toe? (Y or N)")
	}

    	def newBoard(): Array[String] = {
    		return Array(" ", " ", " ", " ", " ", " ", " ", " ", " ")
    	}

    	def getPlayer1(): Name = {
    		readLine("Who wants to be X's? Type your name?")
    	}

    	def getPlayer2(): Name = {
    		readLine("Who wants to be O's? Type your name?")
    	}

    	def getPlayers(player1: Name, player2: Name, random: Int): ((XO, Name),(XO, Name)) = {
    		if(random == 0) {(("X", player1), ("O", player2))} 
    		else {(("O", player2), ("X", player1))}
    	}

    	def coinFlip(): Int = {
    		((System.currentTimeMillis())%2).toInt
    	}

	def printStart(board: Array[String], players: ((XO, Name),(XO, Name))): Unit = {
		println("Flipping coin to see who starts...")
		println(players._1._2 + " to start first")
		printBoard(board)
	}
	
	def printBoard(board: Array[String]): Unit = {
	    	println("Current Board:      Board Positions:")
	    	println("   |   |               |   |        ")
	    	println(" " + board(0) + " | " + board(1) + " | " + board(2) + "           1 | 2 | 3      ")
	    	println("___|___|___         ___|___|___     ")
	    	println("   |   |               |   |        ")
	    	println(" " + board(3) + " | " + board(4) + " | " + board(5) + "           4 | 5 | 6      ")
	    	println("___|___|___         ___|___|___     ")
	    	println("   |   |               |   |        ")
	    	println(" " + board(6) + " | " + board(7) + " | " + board(8) + "           7 | 8 | 9      ")
	    	println("   |   |               |   |        ")
	}

	def playGame(board: Array[String], players: ((XO, Name),(XO, Name))): (XO, Name) = {
    		@annotation.tailrec
    		def loop(currentPlayer: (XO, Name)): (XO, Name) = {
			if (draw(board) == true || win(board) == true) {
				return changeCurrentPlayer(currentPlayer, players)
			} 
			else {
				println(currentPlayer._2 + " (" + currentPlayer._1 + ") to go next")
				printBoard(makePlay(board, requestSpace(board), currentPlayer))
				loop(changeCurrentPlayer(currentPlayer, players))
			}
		}
		loop(players._1)
	}

	def changeCurrentPlayer(currentPlayer: (XO, Name), players: ((XO, Name),(XO, Name))): (XO, Name) = 
	(currentPlayer._1, currentPlayer._2) match  {
 		case (players._1._1, players._1._2) => {var newPlayer = players._2; return newPlayer}
		case (players._2._1, players._2._2) => {var newPlayer = players._1; return newPlayer}
	}

	def makePlay(board: Array[String], space: Int, currentPlayer: (XO, Name)): Array[String] = {
		board(space - 1) = currentPlayer._1
		return board
	}

	def requestSpace(board: Array[String]): Int = {
		var entry = (readLine("Enter space to play"))
		while(validSpace(board, entry) == false){
			entry = (readLine("Invalid entry. Choose a free space and enter digit!"))
		}
		return entry.toInt
	}

	def validSpace(board: Array[String], entry: String): Boolean = {
		if(validEntry(entry) == true) {
			val space = entry.toInt
			((space > 0) && (space < 10) && (board(space - 1) == " "))
		}
		else {false}
	}

	def validEntry(entry: String): Boolean = {
		val nonDigits = ("[^0-9]").r
		(nonDigits findAllIn entry).mkString == ""
	}

	def draw(board: Array[String]) : Boolean = {
      		board.count(_ == " ") == 0
    	}

	def win(board: Array[String]) : Boolean = {
		pWinningLines(board).map { line =>
      		(winningLine(line))
        	} contains true
	}

	def pWinningLines(board: Array[String]) : Array[Array[String]] = {
		var potentialWinningLines: Array[Array[String]] = 
			Array(Array(board(0),board(1),board(2)), 
			Array(board(3),board(4),board(5)),
			Array(board(6),board(7),board(8)),
			Array(board(0),board(3),board(6)),
			Array(board(1),board(4),board(7)),
			Array(board(2),board(5),board(8)),
			Array(board(0),board(4),board(8)),
			Array(board(2),board(4),board(6))) 
		potentialWinningLines
	}

	def winningLine(line: Array[String]) : Boolean = {
      		!line(0).equals(" ") && line(0).equals(line(1)) && line(1).equals(line(2))
    	}

    	def endGame(board: Array[String], lastPlayer: (XO, Name)) = {
		if(win(board) == true){println(lastPlayer._2 + " (" + lastPlayer._1 + ") wins!")}
		else{println("game is a draw")}
    	}	
}
