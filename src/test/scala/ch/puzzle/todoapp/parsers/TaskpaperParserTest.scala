package ch.puzzle.todoapp.parsers

import junit.framework.TestSuite
import _root_.org.junit.Test
import scala.util.parsing.combinator._

class TaskpaperParserTest extends TestSuite {
  
  @Test def testSimpleTask() {
    val input = "- this is a task without tags."
    val parser = new TaskpaperParser
    assert(parser.parseAll(parser.task, input).successful)
  }
  
  @Test def testWrongTask() {
    val input = "this is not a task."
    val parser = new TaskpaperParser
    assert(!parser.parseAll(parser.task, input).successful)
  }
  
  @Test def testSimpleTag() {
    val input = "@tag"
    val parser = new TaskpaperParser
    assert(parser.parseAll(parser.tag, input).successful)
  }
  
  @Test def testTagWithValue() {
    val input = "@tag(i am the value)"
    val parser = new TaskpaperParser
    assert(parser.parseAll(parser.tag, input).successful)
  }
  
  @Test def testNote() {
    val input = "this is a simple note"
    val parser = new TaskpaperParser
    assert(parser.parseAll(parser.note, input).successful)
  }
  
  @Test def testNoNote() {
    val input = "- this is a task and not a note"
    val parser = new TaskpaperParser
    assert(!parser.parseAll(parser.note, input).successful)
  }
  
  @Test def testProject() {
    val input = "this is a project:"
    val parser = new TaskpaperParser
    assert(parser.parseAll(parser.project, input).successful)
  }
  
}