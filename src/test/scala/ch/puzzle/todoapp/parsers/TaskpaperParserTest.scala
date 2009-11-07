package ch.puzzle.todoapp.parsers

import junit.framework.TestSuite
import _root_.org.junit.Test
import _root_.org.junit.Before
import scala.util.parsing.combinator._
import org.junit._
import Assert._

class TaskpaperParserTest extends TestSuite {
  private val parser = TaskpaperParser
  
  @Test def shouldParseSimpleTask() {
    val input = "- this is a task without tags."
    
    assert(parser.parseAll(parser.task, input).successful)
  }
  
  @Test def shouldParseTaskWithTags() {
    val input = "- task @tag1 @tag2 @tag3(this should work as well) @tag4"
    
    assert(parser.parseAll(parser.task, input).successful)
  }
  
  @Test def shouldParseWrongTask() {
    val input = "this is not a task."
    
    assert(!parser.parseAll(parser.task, input).successful)
  }
  
  @Test def shouldParseSimpleTag() {
    val input = "@nameOfTheTag"
    
    assert(parser.parseAll(parser.tag, input).successful)
    assertEquals(Tag("nameOfTheTag", null), parser.parseAll(parser.tag, input).get)
  }
  
  @Test def shouldParseTagWithValue() {
    val input = "@tag(i am the value)"
    assert(parser.parseAll(parser.tag, input).successful)
    assertEquals(Tag("tag", "i am the value"), parser.parseAll(parser.tag, input).get)
  }
  
  @Test def shouldParseNote() {
    val input = "this is a simple note"
    assert(parser.parseAll(parser.note, input).successful)
    assertEquals(Note("this is a simple note", List()), parser.parseAll(parser.note, input).get)
  }
  
  @Test def shouldParseNoNote() {
    val input = "- this is a task and not a note"
    assert(!parser.parseAll(parser.note, input).successful)
  }
  
  @Test def shouldParseProject() {
    val input = "this is a project:"
    
    assert(parser.parseAll(parser.project, input).successful)
  }
  
  @Test def shouldParseTasklistWithTaskAndProject() {
    val input = """the project:
                       |- a task
                       |another project:
                       |- another project's task""".stripMargin

    assert(parser.parseAll(parser.tasklist, input).successful)
  }
  
  @Test def shouldParseProjectWithChildTask() {
	val input = """project:
		           |	- child task
                   |- second child""".stripMargin

    assert(parser.parseAll(parser.member, input).successful)
  }
  
  @Test def shouldParseProjectChildren() {
    val input = """- parent task
		  |	- first child
		  |	a child note
          | - second child
          |project:
          |	- task
          |another project:
          |- task""".stripMargin
    
    assert(parser.parseAll(parser.tasklist, input).successful)
  }
  
  @Test def shouldParseChild() {
    val input = "	child note"
    
    assert(parser.parseAll(parser.child, input).successful)
  }
  
  @Test def shouldParseSimpleBlock() {
    val input = """- task
                  |note
                  |project:""".stripMargin
    
    assert(parser.parseAll(parser.tasklist, input).successful)
  }
  
  @Test def shouldParseText() {
    val input = """text
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

    assert(parser.parseAll(parser.text, input).successful)
  }
   
  @Test def shouldParseTextWithMultipleTaskLists() {
    val input = """text
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
    
    assert(parser.parseAll(parser.text, input).successful)
  }

}