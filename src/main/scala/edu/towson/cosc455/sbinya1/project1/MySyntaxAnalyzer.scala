package edu.towson.cosc455.sbinya1.project1

/**
  * Created by uriel on 10/11/2016.
  */
class MySyntaxAnalyzer extends SyntaxAnalyzer{
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = ???

  override def innerItem(): Unit = ???

  override def innerText(): Unit = ???

  override def link(): Unit = ???

  override def italics(): Unit = ???

  override def body(): Unit = ???

  override def bold(): Unit = ???

  override def newline(): Unit = ???

  override def title(): Unit = ???

  override def variableDefine(): Unit = ???

  override def image(): Unit = ???

  override def variableUse(): Unit = ???

  override def heading(): Unit = ???

  override def listItem(): Unit = ???
}