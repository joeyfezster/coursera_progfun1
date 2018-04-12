/** **********   Lecture 6.3 - Combinatorial Search Example   *************/
object week6_3 {

  //there are 3 fundamental differences between sets and sequences:
  //
  //1. sets are not ordered
  //2. sets do not have duplicates
  //3. the fundamental operation on sets is contains

  //let's use sets to solve the queen's problem

  //a 4x4 board's specific solution of
  // |-*--|
  // |---*|
  // |*---|
  // |--*-|
  //
  //is represented as the list: List(2,0,3,1).
  //note that the head of the list is the last row where a queen was placed

  def solveNQueens(boardDimension: Int): Set[List[Int]] = {
    case class Queen(row: Int, col: Int)

    //i did not refactor placeQueens to use Queen, so that the zip method can be showcased
    //moreover, this should be regarded as the public api of this object
    def placeQueens(queenOrdinal: Int): Set[List[Int]] =
      if (queenOrdinal == 0) Set(List())
      else for {
        otherQueenColumns <- placeQueens(queenOrdinal - 1)
        column <- 0 until boardDimension
        if isQueenSafeFromQueens(Queen(queenOrdinal-1,column), queensFromColumns(otherQueenColumns))
      } yield column :: otherQueenColumns

    def queensFromColumns(columns: List[Int]): Seq[Queen] = {
      val row = columns.size
      (row - 1 to 0 by -1) zip columns map { case (r, c) => Queen(r, c) }
    }

    def isQueenSafeFromQueens(currentQueen: Queen, previousQueens: Seq[Queen]): Boolean = {
      previousQueens.forall(q => isQueenSafeFromQueen(q, currentQueen))
    }

    def isQueenSafeFromQueen(q1: Queen, q2: Queen): Boolean =
      isQueenVerticallySafeFromQueen(q1, q2) && isQueenDiagonallySafeFromQueen(q1, q2)

    def isQueenVerticallySafeFromQueen(q1: Queen, q2: Queen): Boolean = q1.col != q2.col

    def isQueenDiagonallySafeFromQueen(q1: Queen, q2: Queen): Boolean =
      math.abs(q1.col - q2.col) != math.abs(q1.row - q2.row)

    placeQueens(boardDimension)
  }

  def toPrettyString(queenColumns: List[Int]): String = {
    val lines = for(col <- queenColumns.reverse)
      yield Vector.fill(queenColumns.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  solveNQueens(8).map(toPrettyString) mkString "\n"

}
