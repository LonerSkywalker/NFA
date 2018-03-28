package compiler

import scala.io.Source
import scala.util.matching.Regex


object main {
  def readSource(path: String) = Source.fromFile(path).getLines.toList

  def parserline(line:String) = {
    val patternbound = "=|;|,|'|/\\*|\\*/|:|\\(|\\)|\\.".r
    println(patternbound.findAllIn(line).toList map ((x) => (x, "届符")))

    val patternnum = "(\\d+\\.\\d+e\\d+|\\d+\\d+e\\d+|\\d+\\.\\d+|\\d+)".r
    println(patternnum.findAllIn(line).toList map ((x) => (x, "常数")))

    val  patternevl = "not|and|or|\\+|-|\\*|/|<=|>=|==|<>|<|>".r
    println(patternevl.findAllIn(line).toList map ((x) => (x, "运算符")))

    val patternkey = "program|var|integer|bool|real|char|const|begin|if|then|else|while|do|for|to|end|read|write|true|false".r
    println(patternkey.findAllIn(line).toList map ((x) => (x, "关键字")))

  }


  def parser(sourcefile: List[String]): Unit = sourcefile match {
    case line :: rest => {
      parserline(line)
      if (rest.nonEmpty) parser(rest)
    }
    case List() => Nil
  }


  def main(args: Array[String]): Unit = {
    val sourcefile = readSource("/Users/loner/Desktop/test.c")
    parser(sourcefile)
  }
}
