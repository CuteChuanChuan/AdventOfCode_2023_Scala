object CommonUtils {
  
  def getFilePathOrThrowException(args: Array[String]): String = {
    if (args.length != 1) {
      println("Please provide the path to the input file")
      sys.exit(1)
    }
    args(0)
  }
  
}
