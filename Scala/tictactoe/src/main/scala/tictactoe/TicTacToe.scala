package tictactoe

sealed abstract class Piece
case object O extends Piece
case object X extends Piece
case object Blank extends Piece {
  override def toString(): String = "_"
}


class Game(board: List[Piece]) {
  // Alternative constructor for a new game (no pieces played)
  def this() {
    this(List(Blank,Blank,Blank,Blank,Blank,Blank,Blank,Blank,Blank))
  }
  def index(i: Int, j: Int) : Int = (i-1)*3 + j - 1
  def element(i: Int, j: Int): Piece = board(index(i,j))

  // Methods for all the lines on the board
  def row(n: Int): (Piece, Piece, Piece) =
    (element(n,1),element(n,2),element(n,3))

  def column(n: Int): (Piece, Piece, Piece) =
    (element(1,n),element(2,n),element(3,n))

  def diagonal1(): (Piece, Piece, Piece) =
    (element(1,1),element(2,2),element(3,3))

  def diagonal2(): (Piece, Piece, Piece) =
    (element(3,1),element(2,2),element(1,3))

  // Method to test whether all elements of a 3-tuple are equal
  def allSame(line: (Piece, Piece, Piece)): Boolean =
    line._1 == line._2 && line._2 == line._3

  // Method to test if there are any blanks left on the board
  def noBlanks(): Boolean = {
    lines.foreach { line =>
      if ( line._1 == Blank || line._2 == Blank || line._3 == Blank )
        return false
    }
      return true
  }
        
  // lines consists of the tuples representing lines on the board
  // you can win from
  def lines = List( row(1), row(2), row(3),
    column(1), column(2), column(3),
    diagonal1, diagonal2)

  // Who is the winner? X, O or Blank?
  def winner(): Piece = {
    lines.foreach { line =>
      if ( line._1 != Blank && allSame(line) ) return line._1
    }
    Blank
  }

  def state(): String = {
    if (winner != Blank) {
      return (if (winner == X) "X" else "O") + " is the winner"
    } else if ( !noBlanks() ) {
      return "Undecided"
    } else {
      return "Tie"
    }
  }

  def place(p: Piece, i: Int, j: Int): Game = {
    val (left, rightish) = board.splitAt(index(i,j))
    val right = rightish.tail
    val g = new Game( left ++ List(p) ++ right)
    return g
  }

  def printBoard() {
    println(row(1).toString)
    println(row(2).toString)
    println(row(3).toString)
  }

}
