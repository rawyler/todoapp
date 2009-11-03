package ch.puzzle.todoapp

import junit.framework.TestSuite
import _root_.org.junit.Test
import _root_.org.junit.Before
import scala.util.parsing.combinator._
import org.junit._
import Assert._

class TransformerTest extends TestSuite {
  @Test def shouldPassStringToParser() {
    val text = """
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
    	  | text text text""".stripMargin
    
    assertEquals(3, Transformer.parse(text).tasklists.size)
  }
}
