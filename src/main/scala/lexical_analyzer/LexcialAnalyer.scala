package lexical_analyzer

import scala.io.Source


object LexicalAnalyer {
  var buf = ""
  var currentState = "A"
  val letterSet = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  val digitSet = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val blankCharSet = List(' ', '\n', '\t')
  val switchCharSet = List('b', 'n', 't', '\'', '\"', '\\')
  val keyWords = List("auto", "double", "int", "struct", "break", "else", "long", "switch", "case", "enum",
    "register", "typedef", "char", "extern", "return", "union", "const", "float", "short", "unsigned",
    "continue", "for", "signed", "void", "default", "goto", "sizeof", "volatile", "do", "if",
    "while", "static")
  val boardSet = List(';', ',', '(', ')', '.', '{', '}', '[', ']')
  var console_msg = ""

  def compilerFail(status: String): Unit = {
    console_msg += ("编译失败:" + status + '\n')
    currentState = "A"
    buf = ""
  }


  def scanner(text: String): Unit = {
    val ch = text.head
    if (currentState == "A") {
      if (blankCharSet.contains(ch)) {
        currentState = "A"
      } else if (letterSet.contains(ch) | ch == '_') {
        buf += ch
        currentState = "B"
      } else if (digitSet.contains(ch)) {
        buf += ch
        currentState = "C"
      } else if (ch == '\'') {
        buf += ch
        currentState = "D"
      } else if (ch == '\"') {
        buf += ch
        currentState = "G"
      } else if (ch == '/') {
        buf += ch
        currentState = "K"
      } else if (ch == '+') {
        buf += ch
        currentState = "A+"
      } else if (ch == '-') {
        buf += ch
        currentState = "A-"
      } else if (ch == '*') {
        buf += ch
        currentState = "A*"
      } else if (ch == '&') {
        buf += ch
        currentState = "A&"
      } else if (ch == '^') {
        buf += ch
        currentState = "A^"
      } else if (ch == '|') {
        buf += ch
        currentState = "A|"
      } else if (ch == '=') {
        buf += ch
        currentState = "A="
      } else if (ch == '!') {
        buf += ch
        currentState = "A!"
      } else if (ch == '>') {
        buf += ch
        currentState = "A>"
      } else if (ch == '<') {
        buf += ch
        currentState = "A<"
      } else if (boardSet.contains(ch)) {
        buf = ""
        console_msg += ("(" + ch + " , 界符)\n")
      } else if (ch == '$') {
        buf += ch
        currentState = "$"
      }
      else {
        compilerFail("不可识别的字符")
      }


    }
    else if (currentState == "B") {
      if (ch == '_' | letterSet.contains(ch) | digitSet.contains(ch)) {
        buf += ch
        currentState = "B"
      } else {
        if (keyWords.contains(buf)) {
          console_msg += ("(" + buf + ", 关键字)\n")
        } else {
          console_msg += ("(" + buf + ", 标识符)\n")
        }
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "C") {
      if (digitSet.contains(ch)) {
        buf += ch
        currentState = "C"
      } else if (ch == '.') {
        buf += ch
        currentState = "P"
      } else {
        console_msg += ("(" + buf + " , 整数常量)\n")
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "D") {
      if (ch != '\'' & ch != '\\') {
        buf += ch
        currentState = "E"
      } else if (ch != '\'' & ch == '\\') {
        buf += ch
        currentState = "F"
      } else {
        compilerFail("空白或无效的字符")
      }

    }
    else if (currentState == "E") {
      if (ch == '\'') {
        buf += ch
        currentState = "H"
      }
      else {
        compilerFail("字符长度大于1")
      }
    }
    else if (currentState == "H") {
      console_msg += ("(" + buf + " , 字符常量)")
      buf = ""
      currentState = "A"
    }
    else if (currentState == "F") {
      if (switchCharSet.contains(ch)) {
        buf += ch
        currentState = "E"
      } else {
        compilerFail("无效的转义字符")
      }
    }
    else if (currentState == "G") {
      if (ch != '\"' & ch != '\\') {
        buf += ch
        currentState = "G"
      } else if (ch != '\"' & ch == '\\') {
        buf += ch
        currentState = "I"
      } else if (ch == '\"') {
        buf += ch
        currentState = "J"
      }
    }
    else if (currentState == "I") {
      if (switchCharSet.contains(ch)) {
        buf += ch
        currentState = "G"
      } else {
        compilerFail("无效的转义字符")
      }
    }
    else if (currentState == "J") {
      console_msg += "(" + buf + " ,字符串常量)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "K") {
      if (ch == '*') {
        buf = buf.init
        currentState = "L"
      } else if (ch == '/') {
        buf = buf.init
        currentState = "O"
      } else {
        console_msg += "(" + buf + " ,运算符)\n"
      }
    }
    else if (currentState == "L") {
      if (ch != '*') {
        currentState = "L"
      }
      else if (ch == '*') {
        currentState = "M"
      }
    }
    else if (currentState == "M") {
      if (ch == '/') {
        currentState = "N"
      }
      else {
        currentState = "L"
      }
    }
    else if (currentState == "N") {
      currentState = "A"
    }
    else if (currentState == "O") {
      if (ch == '\n') {
        currentState = "A"
      } else
        currentState = "O"
    }
    else if (currentState == "P") {
      if (digitSet.contains(ch)) {
        buf += ch
        currentState = "Q"
      } else {
        compilerFail("无效浮点数")
      }
    }
    else if (currentState == "Q") {
      if (digitSet.contains(ch)) {
        buf += ch
        currentState = "Q"
      } else {
        console_msg += ("(" + buf + ", 浮点数)\n")
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "A+") {
      if (ch == '+' | ch == '=') {
        buf += ch
        currentState = "B+"
      } else {
        console_msg += ('(' + buf + ", 操作符) \n")
        buf = ""
        currentState = "A"
      }

    }
    else if (currentState == "B+") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A-") {
      if (ch == '-' | ch == '=') {
        buf += ch
        currentState = "B-"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B-") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A*") {
      if (ch == '*' | ch == '=') {
        buf += ch
        currentState = "B*"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B(") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A&") {
      if (ch == '&' | ch == '=') {
        buf += ch
        currentState = "B&"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B&") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A^") {
      if (ch == '^' | ch == '=') {
        buf += ch
        currentState = "B^"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B^") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A|") {
      if (ch == '|' | ch == '=') {
        buf += ch
        currentState = "B|"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B|") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A=") {
      if (ch == '=') {
        buf += ch
        currentState = "B="
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B=") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A!") {
      if (ch == '=') {
        buf += ch
        currentState = "B!"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B!") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A>") {
      if (ch == '=') {
        buf += ch
        currentState = "B>"

      } else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B!") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "A<") {
      if (ch == '=') {
        buf += ch
        currentState = "B<"
      }
      else {
        console_msg = console_msg + "(" + buf + " ,操作符)\n"
        buf = ""
        currentState = "A"
      }
    }
    else if (currentState == "B!") {
      console_msg = console_msg + "(" + buf + " ,操作符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "$") {
      console_msg = console_msg + "(" + buf + " ,终结符)\n"
      buf = ""
      currentState = "A"
    }
    else if (currentState == "$") {
      console_msg += "终结"
      buf = ""
      currentState = "A"
    }
    if (text.tail.nonEmpty) {
      scanner(text.tail)
    } else if (text.head != '$') {
      scanner("$")
    }
  }

  def main(args: Array[String]): Unit = {
    val fp = Source.fromFile("src/main/test")
    scanner(fp.mkString)
    println(console_msg)
  }
}
