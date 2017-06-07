package main

import tictactoe._

object Hello extends Greeting with App {
  println(greeting)

  var game = new Game()
  while( game.winner == Blank && !game.noBlanks ) {
    var turn = X


    println("Which row?")
    var row = readInt
    println("Which column?")
    var col = readInt

    
  }
}

trait Greeting {
  lazy val greeting: String = "Tic Tac Toe!"
}
