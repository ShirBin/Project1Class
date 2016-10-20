package edu.towson.cosc455.sbinya1.project1

object Compiler {

  var fileContents : String = ""
  var currentToken : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Scanner = new MySyntaxAnalyzer

  def main(args: Array[String]): Unit = {
    //check usage
    checkFile(args)
    readFile(args(0))


    // gets first token
    Scanner.getNextToken()
    // calls start of BNF in SyntaxAnalyzer
    Parser.gittex()

  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally  source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length !=1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".mkd")) {
      println()
    }
  }
}
