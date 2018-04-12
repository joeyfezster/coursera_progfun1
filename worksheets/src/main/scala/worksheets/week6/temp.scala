import scala.io.{BufferedSource, Source}

/** **********   Lecture 6.5 - Putting the Pieces Together   *************/
object week6_5 {
  val dictionarySource: BufferedSource = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/" +
    "teaching/progfun/linuxwords.txt")

  val words: Iterator[String] = dictionarySource.getLines()

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  val charMap: Map[Char, Char] = reverseMnemonicMapToCharMap()

  def reverseMnemonicMapToCharMap(): Map[Char, Char] = {
    mnem flatMap yieldCharToDigitPairsFromDigitToChars
  }

  def yieldCharToDigitPairsFromDigitToChars(entry: (Char, String)): Seq[(Char,Char)] = {
    val (digit,chars) = entry
    for (char <- chars) yield (char, digit)
  }
}
