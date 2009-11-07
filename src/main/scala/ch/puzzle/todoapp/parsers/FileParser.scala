package ch.puzzle.todoapp.parsers

import scala.collection.mutable.ListBuffer

object FileParser {
  
  def extractBlocks(text: String) = {
    val listBuffer = new ListBuffer[String]
    
    val Block = """(?s)todo[ ]?\{(.*?)\}""".r
    
	Block findAllIn text foreach (_ match {
	  case Block(content) => listBuffer + content
	  case _ => println("NO MATCH")
	})
 
    listBuffer.toList
  }
}
