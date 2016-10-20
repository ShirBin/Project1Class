package edu.towson.cosc455.sbinya1.project1

/**
  * Created by uriel on 10/11/2016.
  */
class MySyntaxAnalyzer extends SyntaxAnalyzer{
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parse tree
      Compiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        // do stuff
      }
    }

    else {
      println("SYNTAX ERROR")
      System.exit(1)
    }
  }

  override def title(): Unit = ???

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def innerText(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = ???

  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def italics(): Unit = ???

  override def listItem(): Unit = ???

  override def innerItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}
