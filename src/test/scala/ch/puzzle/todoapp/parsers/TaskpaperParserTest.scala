package ch.puzzle.todoapp.parsers

import junit.framework.TestSuite
import _root_.org.junit.Test
import _root_.org.junit.Before
import scala.util.parsing.combinator._
import org.junit._
import Assert._

class TaskpaperParserTest extends TestSuite {
  private val parser = new TaskpaperParser
  
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
  
  @Test def shouldParseRootWithTaskAndProject() {
    val input = """todo {
                       |the project:
                       |- a task
                       |another project:
                       |- another project's task
                       |}""".stripMargin
    println(parser.parseAll(parser.root, input))
    assert(parser.parseAll(parser.root, input).successful)
  }
  
  @Test def shouldParseTaskWithChildTask() {
	val input = """- parent task
		           |	- child task
                   |	- second child""".stripMargin
	println(parser.parseAll(parser.member, input))
    assert(parser.parseAll(parser.member, input).successful)
  }
  
  @Test def shouldParseChildren() {
    val input = """todo{
		  |- parent task
		  |	- first child
		  |	a child note
          | - second child
          | 	- child of second
          |project:
          |	- task
		  |}""".stripMargin
    // println(parser.parseAll(parser.root, input))
    assert(parser.parseAll(parser.root, input).successful)
  }
  
  @Test def shouldParseChild() {
    val input = "	child note"
    // println(parser.parseAll(parser.child, input))
    assert(parser.parseAll(parser.child, input).successful)
  }
  
  @Test def shouldNotParseFalseChild() {
    val input = "not a child note"
    assert(!parser.parseAll(parser.child, input).successful)
  }
  
}