package ch.puzzle.todoapp

class Transformer {

}

abstract trait Member {
  val name: String
  val members: List[Member]
}
case class Tag(name: String, value: String) { def this(name: String) = this(name, null) }
case class Task(name: String, tags: List[Tag], members: List[Member]) extends Member
case class Note(name: String, members: List[Member]) extends Member
case class Project(name: String, members: List[Member]) extends Member
case class TaskList(members: List[Member])