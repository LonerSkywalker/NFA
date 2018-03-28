import scala.io.Source

val sourcefile = Source.fromFile("/Users/loner/Desktop/test.c").getLines.toList


//届符
val pattern = "=|;|,|'|/\\*|\\*/|:|\\(|\\)|\\.".r
pattern.findAllIn(sourcefile.head).toList


//常数
val pattern1 = "(\\d+\\.\\d+e\\d+|\\d+\\d+e\\d+|\\d+\\.\\d+|\\d+)".r
pattern1.findAllIn("12sdfa123.1fdsaf12312.3e10").toList



//表示符

//运算符
val  pattern2 = "not|and|or|\\+|-|\\*|/|<=|>=|==|<>|<|>".r
pattern2.findAllIn("notandorfdsaf-kd+sjaa*</>=>=<===n>d").toList


//关键字
val pattern3 = "program|var|integer|bool|real|char|const|begin|if|then|else|while|do|for|to|end|read|write|true|false".r
pattern3.findAllIn("program|var|integer|bool|real|char|const|begin|if|then|else|while|do|for|to|end|read|write|true|false").toList
