package edu.towson.cosc455.sbinya1.project1

/**
  * Created by Shiraz on 10/11/2016.
  */

import scala.collection.mutable.Stack
import java.io._
import java.awt.Desktop
import java.io.{File, IOException}


class MySemanticAnalyzer {
  var outputStack = Stack[String]()
  var parse = Stack[String]()
  var Name = new Array[String](10)
  var varMean = new Array[String](10)
  var nextToken: String = ""
  var output: String = ""
  var counter: Int = 0
  var hasPrinted : Boolean = false
  def semantics(): Unit = {
    //preps for method
    parse = Compiler.Parser.parse.reverse
    nextToken = parse.pop()

    //calls method that process the files
    translator()
  }

  //Checks input to see if it makes sense
  def translator() {
    while (!parse.isEmpty) {
      if (nextToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
        outputStack.push("<html>")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        outputStack.push("<head>")
        outputStack.push("<title>")
        outputStack.push(parse.pop())
        outputStack.push("</title>")
        outputStack.push("</head>")
        parse.pop()
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        outputStack.push("<h1>")
        outputStack.push(parse.pop())
        outputStack.push("</h1>")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        outputStack.push("<p>")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        outputStack.push("</p>")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        outputStack.push("<b>")
        outputStack.push(parse.pop())
        outputStack.push("</b>")
        parse.pop()
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
        outputStack.push("<i>")
        outputStack.push(parse.pop())
        outputStack.push("</i>")
        parse.pop()
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        outputStack.push("<li>")
        nextToken = parse.pop()
        if (nextToken.contains("\n") && !parse.isEmpty && !nextToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
          outputStack.push(nextToken)
        }
        else {
          if(!nextToken.equalsIgnoreCase(CONSTANTS.DOCE))
            translator()
        }
        outputStack.push("</li>")
        if(!parse.isEmpty)
          nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        outputStack.push("<br>")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        val temp = parse.pop()
        parse.pop()
        parse.pop()
        nextToken = parse.pop()
        parse.pop()

        outputStack.push("<a href = \"")
        outputStack.push(nextToken)
        outputStack.push("\">")
        outputStack.push(temp)
        outputStack.push("</a> ")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
        val temp = parse.pop()
        parse.pop()
        parse.pop()
        nextToken = parse.pop()
        parse.pop()

        outputStack.push("<img src =\"")
        outputStack.push(nextToken)
        outputStack.push("\" alt=\"")
        outputStack.push(temp)
        outputStack.push("\">")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        var name = parse.pop()
        parse.pop()
        val mean = parse.pop()
        parse.pop()
        name = name.filter(!" ".contains(_))
        //variables and tokens are passed into the end
        val E_definition = Name.indexOf(name)
        if (E_definition != -1) {
          Name(E_definition) = name
          varMean(E_definition) = mean
        }
        else {
          Name(counter) = name
          varMean(counter) = mean
          counter += 1
        }

        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        var name: String = parse.pop()
        parse.pop()
        name = name.filter(!" ".contains(_))
        //test for var
        if(Name.contains(name))
          outputStack.push(varMean(Name.indexOf(name)))
        else {
          println("SEMANTIC ERROR!: This variable is not defined!")
          System.exit(1)
        }
        //Incrementing variable
        nextToken = parse.pop()
      }

      else if (nextToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        outputStack.push("</html>")
      }
      else {
        outputStack.push(nextToken)
        nextToken = parse.pop()
      }
    }

    //prints output stack to file created
    val output = outputStack.reverse.mkString
    val print = new PrintWriter(new File(Compiler.fileName + ".html"))
    print.write(output)
    print.close

    //calls html to open
    if(!hasPrinted) {
      openInBrowser(Compiler.fileName + ".html")
      hasPrinted = true
    }
  }


  // Opens file in web browser, saw on stackoverflow
  def openInBrowser(htmlFile: String) = {
    val file: File = new File(htmlFile.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFile + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFile)
      case e: Exception => sys.error("ABORT! ABORT! ABORT!")
    }
  }
}
