package ch.puzzle.todoapp.parsers

import scala.util.parsing.combinator._

object TaskpaperParser extends RegexParsers {
  override def skipWhitespace = false
  
  private def space = "[ \\n]+[\t]*".r
  
  private def name = """[^:\n\r\@{}]*""".r
  
  private def tagName = """[\w]*""".r
  
  private def tagIdentifier = "@".r
    
  private def noteText = """[^\n\r{}]*""".r
  
  private def beginList = "todo"~opt(" ")~"{"~opt(space)
  
  private def endList = opt(space)~"}"
  
  private def allowed = not(beginList)~not(endList)~"[^{}]".r
  
  def textContainingTodo: Parser[Text] = ( rep(textBlockContainingTodo) ) ^^
    { case tasklist => Text(tasklist) }
    
  def textBlockContainingTodo: Parser[TaskList] = ( rep(allowed)~beginList~tasklist~endList~rep(allowed) ) ^^
    { case textbefore~begin~tasklist~end~textafter => tasklist }
  
  def tasklist : Parser[TaskList] = ( repsep(member, space) ) ^^
    { case members => TaskList(members) }
  
  def member : Parser[Member] = (project | task | note)
  
  def child : Parser[Member] = (task | note)
  
  /**
   * A task is a line that begins with a hyphen followed by a space ('- ')
   * which can optionally be prefixed (i.e indented) with tabs or spaces.
   * A task can have zero or more context tags.
   */
  def task : Parser[Task] = ( "- "~name~repsep(tag, " ") ) ^^
      { case "- "~name~tags => Task(name.toString, tags, Nil) }

  /**
   * A project is a line that isn't a task and ends with a colon (':')
   * or a colon (':\n') followed by a newline.
   */
  def project : Parser[Project] =
    ( not(task)~name~":"~space~repsep(child, space) ) ^^
      { case t~name~colon~space~members => Project(name.toString, members) } |
    ( not(task)~name~":" ) ^^
      { case t~name~colon => Project(name.toString, Nil) }
  
  /**
  * A note is any line that doesn't match the task or project rules.
  */
  def note : Parser[Note] = ( not(task)~not(project)~not(tag)~noteText ) ^^
      { case task~project~tag~name => Note(name.toString, Nil) }
  
  /**
   * A context tag has the form "@tag", i.e. it starts with an "at" character
   * ("@"), followed by a run of non-whitespace characters. A context tag can
   * optionally have a value assigned to it. The value syntax immediately
   * follows the tag word (no whitespace between) and is enclosed by
   * '(' and ')'. The value text inside can have whitespace, but no newlines.
   */
  def tag : Parser[Tag] = 
    ( tagIdentifier~tagName~"("~"""[^:\n\r\@\(\)]*""".r~")" ) ^^
      { case at~tagName~o~value~c => Tag(tagName.toString, value.toString) } |
    ( tagIdentifier~tagName ) ^^
      { case at~tagName => new Tag(tagName.toString) }

}

abstract trait Member {
  val name: String
  val members: List[Member]
}
case class Tag(name: String, value: String) {
  def this(name: String) = this(name, null)
}

case class Task(name: String, tags: List[Tag], members: List[Member]) extends Member 

case class Note(name: String, members: List[Member]) extends Member

case class Project(name: String, members: List[Member]) extends Member

case class TaskList(members: List[Member])

case class Text(tasklists: List[TaskList])
