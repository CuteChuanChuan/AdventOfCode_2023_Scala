import scala.io.Source
import scala.util.Using

object Colors extends Enumeration {
  type Color = Value
  val red, green, blue = Value
}

object Day02 {
  
  private val bagLimits: Map[String, Int] = Map("red" -> 12, "green" -> 13, "blue" -> 14)
  private val colors: List[String] = List("red", "green", "blue")
  
  private def parseLine(line: String): List[Map[String, Int]] = {
    line.split(": ").last.split("; ").map { set =>
      set.trim.split(",").map { cube =>
        val Array(cnt, color) = cube.trim.split(" ")
        color -> cnt.toInt
      }.toMap
    }.toList
  }
  
  private def minCubeNeeded(game: List[Map[String, Int]]): Map[String, Int] = {
    colors.map { color =>
      color -> game.map(set => set.getOrElse(color, 0)).max
    }.toMap
  }
  
  private def calculatePower(minCube: Map[String, Int]): Int = {
    minCube.values.product
  }
  
  private def isPossible(game: List[Map[String, Int]]): Boolean = {
    game.forall(set => set.forall((color, count) => bagLimits.getOrElse(color, 0) >= count))
  }
  
  def main(args: Array[String]): Unit = {
    val filePath = CommonUtils.getFilePathOrThrowException(args)
    Using(Source.fromFile(filePath)) { source =>
      val games = source.getLines().toList.zipWithIndex.map {
        case (line, idx) => (parseLine(line), idx + 1)
      }
      val possibleGames = games.collect { case (game, idx) if isPossible(game) => idx }
      val sumOfPossibleGameIds = possibleGames.sum
      println(s"Sum of possible game IDs: $sumOfPossibleGameIds")
      
      val powers = games.map {game =>
        val minCube = minCubeNeeded(game._1)
        calculatePower(minCube)
      }
      val sumOfPower = powers.sum
      println(s"Sum of power: $sumOfPower")
    }.recover {
      case e: Exception => println(s"An error occurred: ${e.getMessage}")
    }
  }
}
