package Mustache 

import scala.annotation.tailrec
import scala.io.Source

private class ParserState
private object Text extends ParserState
private object OTag extends ParserState
private object Tag  extends ParserState
private object CTag extends ParserState

private abstract class Parser {
  val src:Source

  var state:ParserState = Text
  var otag:String
  var ctag:String
  var tagPosition:Int = 0
  var line:Int = 1
  var prev:Char = '\uffff'
  var cur:Char = '\uffff'
  var curlyBraceTag:Boolean = false
  var stack:List[Token] = List()

  val buf = new StringBuilder(8192)

  def parse():Token = {
    while(consume) {
      state match {
        case Text =>
          if (cur == otag.charAt(0))
            if (otag.length > 1) { tagPosition = 1; state = OTag } 
            else { staticText; state = Tag }
            else buf.append(cur)

        case OTag =>
          if(cur == otag.charAt(tagPosition))
            if (tagPosition == otag.length-1) { staticText; state = Tag }
            else { tagPosition = tagPosition+1 }
            else { notOTag; buf.append(cur) }

        case Tag =>
          if (buf.isEmpty && cur == '{') {
            curlyBraceTag = true
            buf.append(cur)
          } 
          else if (curlyBraceTag && cur == '}') {
            curlyBraceTag = false
            buf.append(cur)
          } 
          else if (cur == ctag.charAt(0)) {
            if (ctag.length > 1) { tagPosition = 1; state = CTag }
            else tag
          } 
          else buf.append(cur)

        case CTag =>
          if (cur == ctag.charAt(tagPosition)) {
            if (tagPosition == ctag.length-1) tag
            else { tagPosition = tagPosition+1 }
          } 
          else { notCTag; buf.append(cur) }
      }
    }
    state match {
      case Text => staticText
      case OTag => { notOTag; staticText }
      case Tag => fail("Unclosed tag \""+buf.toString+"\"")
      case CTag => { notCTag; staticText }
    }
    stack.foreach {
      case IncompleteSection(key,_,_,_) => fail("Unclosed mustache section \""+key+"\"")
      case _ =>
    }
    val result = stack.reverse

    if(result.size == 1) result(0)
    else RootToken(result)
  }

  private def fail[A](msg:String):A = throw MustacheParseException(line,msg)

  private def consume = {
    prev = cur

    if (src.hasNext) {
      cur = src.next
      // \n, \r\n, \r
      if (cur == '\r' || (cur == '\n' && prev != '\r')) 
        line = line+1
      true
    } 
    else false
  }

  private def notOTag = { 
    buf.append(otag.substring(0,tagPosition))
    state = Text 
  }
  private def notCTag = { 
    buf.append(ctag.substring(0,tagPosition))
    state = Tag
  }
  private def reduce:String = { val r = buf.toString; buf.clear; r }

  private def staticText:Unit = { 
    val r = reduce
    if (r.length>0) 
      stack = StaticTextToken(r)::stack
  }

  private def checkContent(content:String):String = {
    val trimmed = content.trim
    if (trimmed.length == 0) fail("Empty tag")
    else trimmed
  }

  private def tag:Unit = {
    state = Text
    val content = checkContent(reduce)
    def skipFirst = checkContent(content substring 1)
    def skipBoth = checkContent(content substring(1, content.length-1))

    content.charAt(0) match {
      case '!' => // ignore comments
      case '&' =>
        stack = UnescapedToken(skipFirst,otag,ctag)::stack
      case '{' =>
        if (content endsWith "}")
          stack = UnescapedToken(skipBoth,otag,ctag)::stack
        else fail("Unbalanced \"{\" in tag \""+content+"\"")
      case '^' =>
        stack = IncompleteSection(skipFirst, true, otag, ctag)::stack
      case '#' =>
        stack = IncompleteSection(skipFirst, false, otag, ctag)::stack
      case '/' => {
        val name = skipFirst

        @tailrec
        def addSection (
          children:List[Token],
          s:List[Token]) : List[Token] = 
          s.headOption match {
            case None => fail("Closing unopened section \""+name+"\"")

            case Some(IncompleteSection(key, inverted,startOTag,startCTag)) =>
              if (key == name)
                SectionToken(
                  inverted, 
                  name, 
                  children, 
                  startOTag, 
                  startCTag, 
                  otag, 
                  ctag) :: s.tail
              else fail("Unclosed section \""+key+"\"")

            case Some(other) => 
              addSection(other::children, s.tail)
          }
        stack = addSection(List[Token](), stack)
      }
      case '>' | '<' =>
        stack = PartialToken(skipFirst,otag,ctag)::stack
      case '=' =>
        if (content.size>2 && content.endsWith("=")) {
          val changeDelimiter = skipBoth
          changeDelimiter.split("""\s+""",-1).toSeq match {
            case Seq(o,c) => { 
                stack = ChangeDelimitersToken(o,c,otag,ctag)::stack
                otag = o; ctag = c 
              }
            case _ => fail("Invalid change delimiter tag content: \""+changeDelimiter+"\"")
          }
        } 
        else 
          fail("Invalid change delimiter tag content: \""+content+"\"")
            case _ => stack = EscapedToken(content, otag, ctag)::stack
    }
  }
}
