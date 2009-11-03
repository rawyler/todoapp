package ch.puzzle.todoapp

import scala.io.Source
import parsers.TaskpaperParser
import parsers._

object Transformer {
  
  def main(args: Array[String]) {
    for (arg <- args) {
      transform(arg)
    }
  }
  
  def transform(path: String) {
    val lines = Source.fromFile(path).mkString
    
    val text = parse(lines)
    
    text.tasklists.foreach(println)
    
    println("done")
  }
  
  def parse(text: String) = {
    TaskpaperParser.parseAll(TaskpaperParser.text, text).get
  }
  
  def splitIntoTaskpaperParts(text: String) = {
    
  }
  
}