package ch.puzzle.todoapp

import scala.io.Source
import java.io.PrintWriter
import java.io.File
import parsers.TaskpaperParser
import parsers._
import java.util.Date
import java.text.SimpleDateFormat

object Transformer {
  
  final val fileExtension = ".taskpaper"
  
  def main(args: Array[String]) {
    
    args.size match {
      case 1 => transform(args(0))
      case 2 => transform(args(0), args(1))
      case _ => println("illegal number of arguments")
    }
    
  }
  
  def transform(source: String) {
    val now = new Date
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd_hh:mm:ss")
    val dateString = dateFormat format now
    
    transform(source, dateString)
  }
  
  def transform(source: String, destination: String) {
    val lines = Source.fromFile(source).mkString
    
    val taskpaperFile = new File(destination + fileExtension)
    
    val tasklists = FileParser.extractBlocks(lines)
    
    withPrintWriter (taskpaperFile) {
      writer => 
      for (tasklist <- tasklists) {
        if (validTasklist(tasklist)) {
          writer.println(tasklist)
        } else {
          println("error in syntax of tasklist:\n" + tasklist)
        }
      }
    }
    
    println("finished transforming " + source)
    println("output file is " + destination + fileExtension)
  }
  
  def validTasklist(tasklist: String) =
    TaskpaperParser.parseAll(TaskpaperParser.tasklist, tasklist).successful
  
  def withPrintWriter(file: File)(op: PrintWriter => Unit) {
    val writer = new PrintWriter(file)
    try {
      op(writer)
    } finally {
      writer.close()
    }
  }
  
}