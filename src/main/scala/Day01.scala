import scala.io.Source
import scala.util.Using

object Day01 {
  
  private val wordToDigit: Map[String, Int] = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )
  
  private def convertToDigit(line: String): String = {
    var remainingLine = line
    var result = ""
    while (remainingLine.nonEmpty) {
      val matchedWord = wordToDigit.keys.find(word => remainingLine.startsWith(word))
      matchedWord match
        case Some(word) => result += wordToDigit(word)
        case None => if (remainingLine.head.isDigit) result += remainingLine.head
      remainingLine = remainingLine.tail
    }
    result
  }
  
  private def getCalibrationValue(line: String): Option[Int] = {
    val digits = convertToDigit(line)
    val result = digits.length match
      case 1 => Some((digits.head.toString + digits.head.toString).toInt)
      case _ if digits.length >= 2 => Some((digits.head.toString + digits.last.toString).toInt)
      case _ => None
    result
  }
  
  def main(args: Array[String]): Unit = {
    val filePath = CommonUtils.getFilePathOrThrowException(args)
    
    val totalSum = Using.resource(Source.fromFile(filePath)) { source =>
      source.getLines().flatMap(getCalibrationValue).sum
    }
    println(totalSum)
  }
}
