package main

import tictactoe._

object Hello extends Greeting with App {
  println(greeting)

  var game = new Game()
  var turn: Piece = X
  while( game.winner == Blank && !game.noBlanks ) {


    println("Which row?")
    var row = readInt
    println("Which column?")
    var col = readInt

    game = game.place(turn, row, col)
    game.printBoard()
    if ( turn == X ) { turn = O } else { turn = X }
  }
  println(game.state)
}

trait Greeting {
  lazy val greeting: String = "Tic Tac Toe!"
}
