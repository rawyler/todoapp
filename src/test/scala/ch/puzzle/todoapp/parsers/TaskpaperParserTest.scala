package ch.puzzle.todoapp.parsers

import junit.framework.TestSuite
import _root_.org.junit.Test
import _root_.org.junit.Before
import scala.util.parsing.combinator._

class TaskpaperParserTest extends TestSuite {
  private val parser = new TaskpaperParser
  
  @Test def testSimpleTask() {
    val input = "- this is a task without tags."
    assert(parser.parseAll(parser.task, input).successful)
  }
  
  @Test def testWrongTask() {
    val input = "this is not a task."
    assert(!parser.parseAll(parser.task, input).successful)
  }
  
  @Test def testSimpleTag() {
    val input = "@tag"
    assert(parser.parseAll(parser.tag, input).successful)
  }
  
  @Test def testTagWithValue() {
    val input = "@tag(i am the value)"
    assert(parser.parseAll(parser.tag, input).successful)
  }
  
  @Test def testNote() {
    val input = "this is a simple note"
    assert(parser.parseAll(parser.note, input).successful)
  }
  
  @Test def testNoNote() {
    val input = "- this is a task and not a note"
    assert(!parser.parseAll(parser.note, input).successful)
  }
  
  @Test def testProject() {
    val input = "this is a project:"
    assert(parser.parseAll(parser.project, input).successful)
  }
  
  @Test def testRootWithTaskAndProject() {
    val input = """todo {
                       |the project:
                       |- a task
                       |another project:
                       |	- another project's task
                       |}""".stripMargin
    assert(parser.parseAll(parser.root, input).successful)
  }
  
}