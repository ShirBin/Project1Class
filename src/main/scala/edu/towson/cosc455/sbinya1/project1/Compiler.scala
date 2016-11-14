package edu.towson.cosc455.sbinya1.project1

// Shiraz Binyamin COSC455
object Compiler {

  var currentToken : String = ""
  var fileName: String = ""
  var fileContents : String = ""
  var closeFile : Boolean = false


  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val Semantic = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))

    // Reads file, processes, and creates a string
    println("File has passed in system: ")
    print(fileContents)
    println()
    println("Processing: " )

    //Scanner/Lexical Analysis
    Scanner.start (fileContents)

    while (Scanner.fileLocation < Scanner.fileSize && !closeFile) {

      //Goes to next analyzer if passes lexical
      Scanner.getNextToken()

      // Goes through MySyntax analyzer
      Parser.gittex()

      if (currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        closeFile = true;
      }
      // Goes to semantic after being cleared with syntax
    }
    Semantic.semantics()

  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".mkd")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
