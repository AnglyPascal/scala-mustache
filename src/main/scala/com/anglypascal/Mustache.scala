package com.anglypascal.mustache 

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.Map

import com.rallyhealth.weejson.v1._
import com.rallyhealth.weejson.v1.jackson.FromJson
import com.rallyhealth.weepickle.v1.WeePickle._

import scala.reflect.runtime.universe._

/**
 * view helper trait 
 **/
trait MustacheHelperSupport {
  private val contextLocal = new java.lang.ThreadLocal[Any]()
  private val renderLocal  = new java.lang.ThreadLocal[Function1[String,String]]()

  protected def context: Any = contextLocal.get
  protected def render(template: String): Any = (renderLocal.get())(template)

  // TODO: understand the effects of contextLocal and renderLocal
  // TODO: understand the callstack 
  def withContextAndRenderFn[A]
    (context: Any, render: (String) => String)
    (fn: => A): A = {

    contextLocal.set(context)
    renderLocal.set(render)
    try fn 
    finally { 
      contextLocal.set(null)
      renderLocal.set(null) 
    }
  }
}

case class MustacheParseException(line: Int, msg: String) 
  extends Exception("Line " + line + ": " + msg)

/** Mustache object to represent a Mustache template 
 *  Provides render method to render the template
 */
class Mustache(root: Token) 
  extends MustacheHelperSupport with Obj2Scala {

  def this(source: Source, open: String = "{{", close: String = "}}") =
    this((new Parser {
        val src  = source
        var otag = open
        var ctag = close
      }).parse()
    )

  def this(str: String) = 
    this(Source.fromString(str))

  def this(str: String, open: String, close: String) = 
    this(Source.fromString(str), open, close)

  private val compiledTemplate: Token = root

  /** Get the methods defined in the instance of a Mustache class
   *  that are of the type Function0[_] or Function1[String, _] 
   */
  val globals: Map[String, Any] = {
    val excludedGlobals = 
      List("wait", "toString", "hashCode",
        "getClass", "notify", "notifyAll")

    /** filters methods that we don't want
     */
    def filterMethod(x: java.lang.reflect.Method): Boolean = {
      val name = x.getName 
      val pt = x.getParameterTypes

      !name.startsWith("render$default") && 
      !name.startsWith("product$default") && 
      !name.startsWith("init$default") && 
      !excludedGlobals.contains(name) && 
      ((pt.length == 0) || (pt.length == 1 && pt(0) == classOf[String]))
    }

    def mapMethod(x: java.lang.reflect.Method): (String, Object) = {
      x.getName -> (
        if(x.getParameterTypes.length == 0) 
          () => { x.invoke(this) }
        else 
          (str: String) => { x.invoke(this, str) }
      )
    }

    Map((this.getClass().getMethods().filter(filterMethod).map(mapMethod)) : _*)
  }

  /** the render function that is called on this template with 
   *  context holding the values to be mapped
   *  partials holding the map from tags to mustache templates
   *  callstack holding the stack of all templates still waiting to be rendered
   */
  def render(
    context: Any                    = null,
    partials: Map[String, Mustache] = Map(),
    callstack: List[Any]            = List(this)): String = {
      context match {
        case str: String => {
          val obj = FromJson(str).transform(Value)
          product(val2Scala(obj), partials, callstack).toString
        }
        case obj: Value => 
          product(val2Scala(obj), partials, callstack).toString
        case other =>
          product(other, partials, callstack).toString
      }

  }

  /** returns a TokenProduct with the rendered template
   */
  def product(
    context: Any                    = null,
    partials: Map[String, Mustache] = Map(),
    callstack: List[Any]            = List(this)) : TokenProduct =
      compiledTemplate.render(context, partials, callstack)

}


object Mustache{
  def main(args : Array[String]) : Unit = {
    val template = new Mustache("Hello, {{ #name }}{{haha}}{{ /name }}!") {
      def name = List( Map("haha" -> "Ah"), Map("haha" -> "san") )
    }
    
    println(template.render())
  }

}
