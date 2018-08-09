import scala.io.Source

/** **********   Lecture 6.5 - Putting the Pieces Together   *************/
object week6_5 {
  val dictionarySource = Source.fromURL("https://lamp.epfl" +
    ".ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

  val words = cleanWordList(dictionarySource.getLines().toList)

  def cleanWordList(words: Seq[String]) = words.filter(word => word.forall(chr => chr.isLetter))

  val digitToChars = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  val charToDigit: Map[Char, Char] = getCharToDigit

  def getCharToDigit: Map[Char, Char] = {
    for {
      (digit, str) <- digitToChars
      //note how you can generate from results of previous generators
      letter <- str
    } yield letter -> digit
  }

  def getDigitStringFromWord(word: String): String = word.toUpperCase.map(charToDigit)

  val wordsByNumericEncoding: Map[String, Seq[String]] =
    words.groupBy(getDigitStringFromWord).withDefaultValue(Seq())

  def getAllMnemonicRepresentationsOfNumber(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        prefixLength <- 1 to number.length
        firstWord <- wordsByNumericEncoding(number take prefixLength)
        otherWords <- getAllMnemonicRepresentationsOfNumber(number drop prefixLength)
      } yield firstWord :: otherWords
    }.toSet


  //This would be the public method in this API
  def translate(number: String): Set[String] =
    getAllMnemonicRepresentationsOfNumber(number).map(_ mkString " ")

  translate("7225247386")
}
