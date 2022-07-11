package Mustache 

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.Map


case class MustacheParseException(line:Int, msg:String) 
extends Exception("Line "+line+": "+msg)

/**
 * template
 **/
class Mustache(root: Token) extends MustacheHelperSupport {

  def this (
    source:Source, 
    open:String = "{{", 
    close:String = "}}") =
    this(
      (new Parser{
        val src = source
        var otag = open
        var ctag = close
      }).parse())

  def this(str:String) = this(Source.fromString(str))

  def this(
    str:String, 
    open:String, 
    close:String) = this(Source.fromString(str), open, close)

  private val compiledTemplate = root

  val globals: Map[String,Any] = {
    val excludedGlobals = List("wait","toString","hashCode",
      "getClass","notify","notifyAll")
    Map( 
      (this.getClass().getMethods
        .filter(x => {
          val name = x.getName 
          val pt = x.getParameterTypes
          (!name.startsWith("render$default")) && 
          (!name.startsWith("product$default")) && 
          (!name.startsWith("init$default")) && 
          (!excludedGlobals.contains(name)) && 
          ((pt.length == 0) || (pt.length == 1 && pt(0) == classOf[String]))
        })
        .map( x=>{x.getName-> 
          (if(x.getParameterTypes.length == 0) ()=>{ x.invoke(this) }
            else (str:String)=>{ x.invoke(this, str) })
        })) : _*
    )
  }

  def render(
    context : Any = null, 
    partials : Map[String,Mustache] = Map(), 
    callstack : List[Any] = List(this)) : String = 
      product(context, partials, callstack).toString

  def product(
    context : Any = null, 
    partials : Map[String,Mustache] = Map(), 
    callstack : List[Any] = List(this)) : TokenProduct = 
      compiledTemplate.render(context, partials, callstack)

}


object Bruh{
  def main(args : Array[String]) : Unit = {
    val template = new Mustache("Hello, {{ name }}!")
    println(template.render(Map("name"->"world")))

    val userTemplate = new Mustache("<strong>{{name}}</strong>")
    val baseTemplate = new Mustache(
      "<h2>Names</h2>\n{{#names}}\n  {{> user}}\n{{/names}}"
    )
    val ctx = Map("names"->List(
      Map("name"->"Alice")
    ,Map("name"->"Bob")
  ))
    val partials = Map("user" -> userTemplate)
    println(baseTemplate.render(ctx, partials))
  }

}
