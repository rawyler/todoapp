package ch.puzzle.todoapp.parsers

import scala.util.parsing.combinator._

class TaskpaperParser extends JavaTokenParsers {
  override def skipWhitespace = false
  
  private def space = "[ \\n]+".r
  
  def root : Parser[Any] = "todo"~opt(" ")~"{"~space~rep(member)~"}"
  
  def member : Parser[Any] = (project | task | note)~space
  
  /**
   * A task is a line that begins with a hyphen followed by a space ('- ')
   * which can optionally be prefixed (i.e indented) with tabs or spaces.
   * A task can have zero or more context tags.
   */
  def task : Parser[Any] = "- "~name~rep(tag)

  /**
   * A project is a line that isn't a task and ends with a colon (':')
   * or a colon (':\n') followed by a newline.
   */
  def project : Parser[Any] = not(task)~name~":"
  
  /**
  * A note is any line that doesn't match the task or project rules.
  */
  def note : Parser[Any] = not(task)~not(project)~not(tag)~noteText
  
  /**
   * A context tag has the form "@tag", i.e. it starts with an "at" character
   * ("@"), followed by a run of non-whitespace characters. A context tag can
   * optionally have a value assigned to it. The value syntax immediately
   * follows the tag word (no whitespace between) and is enclosed by
   * '(' and ')'. The value text inside can have whitespace, but no newlines.
   */
  def tag : Parser[Any] = tagName~opt(value)
  
  /**
   * The value of a tag
   */
  def value : Parser[Any] = name
  
  /**
  * Name for project and task
  */
  private def name : Parser[String] = """[^:\n\r\@]*""".r
  
  private def tagName : Parser[String] = """\@[\w]*""".r
    
  private def noteText : Parser[String] = """[^\n\r]*""".r
}
