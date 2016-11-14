package edu.towson.cosc455.sbinya1.project1

/**
  * Created by Shiraz on 10/11/2016.
  */


import scala.collection.mutable.Stack


class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var parse = Stack[String]()
  var found: Boolean = false;

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

  override def paragraph(): Unit = {
    parb()
    variableDefine()
    while (!Compiler.currentToken.equals(CONSTANTS.DOCE) && !Compiler.currentToken.equals(CONSTANTS.PARAE)) {
      innerText()
    }
    pare()
  }


  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
      italics()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      found = true
    }
    else if (Compiler.Scanner.isText(Compiler.currentToken)) {
      text()
      innerItem()
    }
    else
      found = true
  }

  override def innerText(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      if(parse.contains(CONSTANTS.PARAB)){
        pare()
      }
      else {
        println("SYNTAX ERROR!: \\PARAB was never defined")
        System.exit(1)
      }
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
      italics()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      return
    }
    else
      posText()
  }

  override def link(): Unit = {
    Lbrace()
    text()
    Rbrace()
    Lparanthesis()
    text()
    Rparanthesis()
  }

  override def italics(): Unit = {
    italb()
    posText()
    italb()
  }

  override def body(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      variableDefine()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {

    }
    else {
      innerText()
      body()
    }
  }

  override def bold(): Unit = {
    bold()
    posText()
    bold() //@@@
  }


  override def title(): Unit = {
    titleb()
    text()
    Rbrace()
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      defb()
      text()
      equals()
      text()
      Rbrace()
    }
  }

  override def image(): Unit = {
    begin_image()
    text()
    Rbrace()
    Lparanthesis()
    text()
    Rparanthesis()
  }

  override def variableUse(): Unit = {
    begin_use()
    text()
    Rbrace()
  }

  override def heading(): Unit = {
    headb()
    text()
  }

  override def listItem(): Unit = {
    listItemb()
    innerItem()
  }
  //
  //
  //
  //
  //
  //Syntax for methods
  //
  //
  //
  //
  //

  def text(): Unit = {
    if (Compiler.Scanner.isText(Compiler.currentToken)) {
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: Text was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def posText(): Unit = {
    if (Compiler.Scanner.isText(Compiler.currentToken)) {
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  override def newline(): Unit =
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parse.push(CONSTANTS.NEWLINE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\\\ was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }

  def Lbrace(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("[")) {
      parse.push("[")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: [ was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def Rbrace(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("]")) {
      parse.push("]")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: ] was expected to end the token")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def Lparanthesis(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
      parse.push("(")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: ( was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def Rparanthesis(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(")")) {
      parse.push(")")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: ) was expected to end the token")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def equals(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.EQSIGN)) {
      parse.push("=")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: = was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  //specific syntax defs for methods
  def docb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parse.push(CONSTANTS.DOCB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\BEGIN was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def doce(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      parse.push(CONSTANTS.DOCE)
      Compiler.Scanner.getNextToken()
      if (Compiler.Scanner.nextCharacter.equals('\n'))
        return
      else {
        println("SYNTAX ERROR!: items after end of document")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR!: \\END was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def titleb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parse.push(CONSTANTS.TITLEB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\TITLE[ was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def headb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parse.push(CONSTANTS.HEADING)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: # was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def parb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parse.push(CONSTANTS.PARAB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\PARAB was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def pare(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      parse.push(CONSTANTS.PARAE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\PARAE was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def boldb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parse.push(CONSTANTS.BOLD)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: ** was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def italb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
      parse.push(CONSTANTS.ITALICS)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: * was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def listItemb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parse.push(CONSTANTS.LISTITEM)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: + was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def begin_image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parse.push(CONSTANTS.IMAGEB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: ![ was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def begin_use(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parse.push(CONSTANTS.USEB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\USE[ was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

  def defb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parse.push(CONSTANTS.DEFB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR!: \\DEF[ was expected")
      println(Compiler.currentToken + "was discovered")
      System.exit(1)
    }
  }

}