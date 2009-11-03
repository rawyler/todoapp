package ch.puzzle.todoapp.parsers

import junit.framework.TestSuite
import _root_.org.junit.Test
import _root_.org.junit.Before
import scala.util.parsing.combinator._
import org.junit._
import Assert._

class FileParserTest extends TestSuite {
  
  @Test def shouldResultInTaskpaperString() {
    val text = """text
    	  | text text text
          |todo {
    	  |- parent task
		  |	- first child
		  |	a child note
          | - second child
          |project:
          |	- task
          |another project:
          |- task
          |}
    	  | text text text
    	  | text text text
          |todo {
    	  |- parent task
		  |	- first child
		  |	a child note
          | - second child
          |project:
          |	- task
          |another project:
          |- task
          |}
    	  | text text text
    	  | text text text
          |todo {
    	  |- parent task
		  |	- first child
		  |	a child note
          | - second child
          |project:
          |	- task
          |another project:
          |- task
          |}
    	  | text text text
    	  | text text text""".stripMargin
    
    //println(parser.parseAll(parser.text, text).get)
    
    
    println(text)
    println(m(text))
    
    assert(true)
  }
  
  def m(someString: String) = {
    val RE = """([a-z]+)\s*""".r
    
    someString match {
      case RE(block) => block
      case _ => "no match"
    }
  }

}
