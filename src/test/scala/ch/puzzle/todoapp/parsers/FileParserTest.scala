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
    	  |- parent task group one
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
    	  |- task group two
          | - second child
          |project:
          |	- task
          |another project:
          |- task
          |}
    	  | text text text
    	  | text text text
          |todo{
    	  |- parent task group 3
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
    
    val resultList = FileParser.extractBlocks(text)
    
    assertEquals(3, resultList.size)
  }

}
