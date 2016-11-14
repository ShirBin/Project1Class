package edu.towson.cosc455.sbinya1.project1

import scala.collection.mutable.ArrayBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {

  val search = new Array[String](20)
  var token = new ArrayBuffer[Char](50)
  var fileHolder: Array[Char] = Array()
  var nextCharacter: Char = ' '
  var fileLocation: Int = -1
  var fileSize: Int = 0

  //starter method that primes getnexttoken
  def start(file1: String): Unit = {
    SearchArray()
    fileHolder = file1.toCharArray
    fileSize = fileHolder.length - 1
  }

  //Collects the data of characters
 override def getChar(): Unit = {
    if (fileLocation < fileSize) {
      fileLocation += 1
      nextCharacter = fileHolder.charAt(fileLocation)
    }
    else
      return;
  }

  //Grabs another character
  override def addChar() {
    token += nextCharacter
  }

  //collects directory of tokens and will reject any characters
  //Dehlinger says no cases, avoid cases and instead do if else statements
  override def getNextToken(): Unit = {
    getChar()
    getNotText()

    //separates single tokens that are given
    if (nextCharacter.equals('+') || nextCharacter.equals('=') || nextCharacter.equals('#') || nextCharacter.equals('(') || nextCharacter.equals(')') || nextCharacter.equals(']') || nextCharacter.equals('[')) {
      addChar()
    }
    //breaks for next tokens
    else if (nextCharacter.equals('\\')) {
      addChar()
      getChar()
      while (!nextCharacter.equals('[') && nextCharacter != '\r' && nextCharacter != '\n' && nextCharacter != '\\') {
        if (nextCharacter.equals('\r')) {
          addChar()
        }
        else {
          addChar()
          getChar()
        }
      }
      if (nextCharacter.equals('[')) {
        addChar()
      }
      if (nextCharacter.equals('\\')) {
        addChar()
      }
    }

    else if (nextCharacter.equals('*')) {
      addChar()
      getChar()
      if (nextCharacter.equals('*')) {
        addChar()
        getChar()
        compact()
      }
      else {
        fileLocation -= 1
      }
    }
    else if (nextCharacter.equals('!')) {
      addChar()
      getChar()
      if (nextCharacter.equals('[')) {
        addChar()
      }
    }
    else {
      addChar()
      getChar()
      while (!CONSTANTS.TOKENS.contains(nextCharacter)) {
        addChar()
        if (fileLocation < fileSize) {
          getChar()
        }
        else {
          return
        }
      }
      fileLocation -= 1
    }
    compact()
  }


  // Checks for next variable to be a token or not
  def isText(text: String): Boolean = {
    if (text.contains("\\"))
      return false
    else
      return true
  }

  //compact string if it passes lexical analyzer
  def compact(): Unit = {
    val practicalTokenPresent: String = token.mkString
    if (search.contains(practicalTokenPresent.toUpperCase)) {
      println(practicalTokenPresent)
      setCurrentToken(practicalTokenPresent)
      token.clear()
    }
    else if (isText(practicalTokenPresent)) {
      println(practicalTokenPresent)
      setCurrentToken(practicalTokenPresent)
      token.clear()
    }
    else {
      println("LEXICAL ERROR!: Token given is incorrect")
      println(practicalTokenPresent + "was discovered")
      System.exit(1)
    }
  }

  //Moves from one token to another
  def setCurrentToken(currentToken: String): Unit = {
    Compiler.currentToken = currentToken
  }

  def getNotText(): Unit = {
    while (nextCharacter.equals(' ') || nextCharacter.equals('\r') || nextCharacter.equals('\n') || nextCharacter.equals('\t')) {
      getChar()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        getChar()
        return
      }
    }
  }

  //defines search
  def SearchArray() = {
    search(0) = "\\BEGIN";
    search(1) = "\\END";
    search(2) = "\\TITLE[";
    search(3) = "]";
    search(4) = "#";
    search(5) = "\\PARAB";
    search(6) = "\\PARAE";
    search(7) = "\\DEF[";
    search(8) = "\\USE[";
    search(9) = "**";
    search(10) = "*";
    search(11) = "+";
    search(12) = "\\\\";
    search(13) = "[";
    search(14) = "(";
    search(15) = ")";
    search(16) = "=";
    search(17) = "![";
    search(18) = "]";
    search(19) = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toString()
  }
}

