package Mustache 

import scala.annotation.tailrec
import scala.collection.Map
import scala.concurrent.{Await, Awaitable}
import scala.concurrent.duration._

// mustache tokens ------------------------------------------
trait TokenProduct {
  val maxLength:Int
  def write(out:StringBuilder):Unit

  override def toString = {
    val b = new StringBuilder(maxLength)
    write(b)
    b.toString
  }
}

object EmptyProduct extends TokenProduct {
  val maxLength = 0 
  def write(out:StringBuilder):Unit = {}
}

case class StringProduct(str:String) extends TokenProduct {
  val maxLength = str.length 
  def write(out:StringBuilder):Unit = out.append(str)
}


trait Token {
  def render(context:Any
    , partials:Map[String,Mustache]
    , callstack:List[Any]):TokenProduct
  def templateSource:String
}

trait CompositeToken {
  def composite(
    tokens:List[Token], 
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct = 
      composite(tokens.map{(_,context)},partials, callstack)

    def composite(
      tasks:Seq[Tuple2[Token,Any]], 
      partials:Map[String,Mustache], 
      callstack:List[Any]):TokenProduct = {
      val result = tasks.map(t=>{t._1.render(t._2, partials, callstack)})
      val len = result.foldLeft(0)({_+_.maxLength})
      new TokenProduct {
        val maxLength = len
        def write(out:StringBuilder) = result.map{_.write(out)}
      }
    }
}

case class RootToken(children:List[Token]) 
extends Token with CompositeToken {
  private val childrenSource = children.map(_.templateSource).mkString

  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct =
      composite(children, context, partials, callstack)

  def templateSource:String = childrenSource
}

case class IncompleteSection(
  key:String, 
  inverted:Boolean, 
  otag:String, 
  ctag:String) extends Token {
  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct = fail
  def templateSource:String = fail

  private def fail = 
    throw new Exception("Weird thing happened. " 
      + "There is incomplete section in compiled template.")

}

case class StaticTextToken(staticText:String) extends Token {
  private val product = StringProduct(staticText)

  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct = product

  def templateSource:String = staticText
}

case class ChangeDelimitersToken(
  newOTag:String, 
  newCTag:String, 
  otag:String, 
  ctag:String) extends Token {
  private val source = otag + "=" + newOTag + " " + newCTag + "=" + ctag

  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct = EmptyProduct 

  def templateSource:String = source
}

case class PartialToken(
  key:String, 
  otag:String, 
  ctag:String) extends Token {
  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct =
      partials.get(key) match {
        case Some(template) => 
          template.product(context, partials, template::callstack)
        case _ => 
          throw new IllegalArgumentException("Partial \""+key+"\" is not defined.")
      }
  def templateSource:String = otag+">"+key+ctag
}

trait ContextHandler {

  protected def defaultRender(
    otag:String, 
    ctag:String):(Any,Map[String,Mustache],List[Any])=>(String)=>String = 
      (context:Any, partials:Map[String,Mustache],callstack:List[Any])=>(str:String)=>{
        val t = new Mustache(str, otag, ctag)
        t.render(context, partials, callstack)
      }

  def valueOf(
    key:String, 
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any], 
    childrenString:String, 
    render: (Any, Map[String, Mustache],List[Any])=>(String)=>String):Any = {
      val r = render(context, partials, callstack)
      val wrappedEval = callstack
        .filter(_.isInstanceOf[Mustache]).asInstanceOf[List[Mustache]]
        .foldLeft( () =>{ 
          eval(findInContext(context::callstack, key), 
          childrenString, r) })( 
            (f,e) => { ()=>{e.withContextAndRenderFn(context,r)(f())} } )
      wrappedEval() match {
        case None if (key == ".") => context
        case other => other
      }
    }
    

  @tailrec
  private def eval(
    value:Any, 
    childrenString:String, 
    render:(String)=>String ):Any =
      value match {
        case Some(someValue) => eval(someValue, childrenString, render)

        case a:Awaitable[_] =>
          eval(Await.result(a, Duration.Inf), childrenString, render)

        case f:Function0[_] => 
          eval(f(), childrenString, render)

        case s:Seq[_] => s

        case m:Map[_, _] => m

        case f:Function1[String, _] => 
          eval(f(childrenString), childrenString, render)

        case f:Function2[String, Function1[String,String], _] => 
          eval(f(childrenString, render), childrenString, render)

        case other => other
      }

  @tailrec
  private def findInContext(stack:List[Any], key:String):Any =
    stack.headOption match {
      case None => None
      case Some(head) =>
        (head match {
          case null => None
          case m: Map[String, _] =>
            m.get(key) match {
              case Some(v) => v
              case None => None
            }
          case m:Mustache =>
            m.globals.get(key) match {
              case Some(v) => v
              case None => None
            }
          case any => reflection(any, key)
        }) match {
          case None => findInContext(stack.tail, key)
          case x => x
        }
    }

  private def reflection(x:Any, key:String):Any = {
    val w = wrapped(x)
    (methods(w).get(key), fields(w).get(key)) match {
      case (Some(m), _) => m.invoke(w)
      case (None, Some(f)) => f.get(w)
      case _ => None
    }
  }

  private def fields(w:AnyRef) = Map( 
    w.getClass().getFields.map(x => {x.getName -> x}):_*
  )

  private def methods(w:AnyRef) = Map(
    w.getClass().getMethods
      .filter(x => { x.getParameterTypes.length == 0 })
      .map(x => { x.getName -> x }) :_*
  )

  private def wrapped(x:Any):AnyRef =
    x match {
      case x: Byte => byte2Byte(x)
      case x: Short => short2Short(x)
      case x: Char => char2Character(x)
      case x: Int => int2Integer(x)
      case x: Long => long2Long(x)
      case x: Float => float2Float(x)
      case x: Double => double2Double(x)
      case x: Boolean => boolean2Boolean(x)
      case _ => x.asInstanceOf[AnyRef]
    }
}

trait ValuesFormatter {
  @tailrec
  final def format(value:Any):String =
    value match {
      case null => ""
      case None => ""
      case Some(v) => format(v)
      case x => x.toString
    }
}

case class SectionToken(
  inverted:Boolean,
  key:String,
  children:List[Token],
  startOTag:String,
  startCTag:String,
  endOTag:String,
  endCTag:String) 
extends Token with ContextHandler with CompositeToken {

  private val childrenSource = children.map(_.templateSource).mkString

  private val source = startOTag + (if(inverted) "^" else "#") + key + 
  startCTag + childrenSource + endOTag + "/" + key + endCTag

  private val childrenTemplate = {
    val root = if(children.size == 1) children(0)
    else RootToken(children)
    new Mustache( root )
  }

  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct =
      valueOf(
        key, 
        context, 
        partials, 
        callstack, 
        childrenSource, 
        renderContent) match {
          case null => 
            if (inverted) composite(children, context, partials, context::callstack)
            else EmptyProduct
          case None => 
            if (inverted) composite(children, context, partials, context::callstack)
            else EmptyProduct
          case b:Boolean => 
            if (b^inverted) composite(children, context, partials, context::callstack)
            else EmptyProduct
          case s:Seq[_] if(inverted) => 
            if (s.isEmpty) composite(children, context, partials, context::callstack)
            else EmptyProduct
          case s:Seq[_] if(!inverted) => {
            val tasks = for (element<-s;token<-children) yield (token, element)
            composite(tasks, partials, context::callstack)
          }
          case str:String => 
            if (!inverted) StringProduct(str)
            else EmptyProduct

          case other => 
            if (!inverted) composite(children, other, partials, context::callstack)
            else EmptyProduct
        }

  private def renderContent(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]) (template:String) : String =
      // it will be children nodes in most cases
      // TODO: some cache for dynamically generated templates?
      if (template == childrenSource)
        childrenTemplate.render(context, partials, context::callstack)
      else {
        val t = new Mustache(template, startOTag, startCTag)
        t.render(context, partials, context::callstack)
      }

  def templateSource:String = source
}

case class UnescapedToken(key:String, otag:String, ctag:String) 
extends Token with ContextHandler with ValuesFormatter {
  private val source = otag + "&" + key + ctag

  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct = {
      val v = format(valueOf(key,context,partials,callstack,"",defaultRender(otag,ctag)))
      new TokenProduct {
        val maxLength = v.length
        def write(out:StringBuilder):Unit = {out.append(v)}
      }
  }

  def templateSource:String = source
}

case class EscapedToken(key:String, otag:String, ctag:String) 
extends Token with ContextHandler with ValuesFormatter {
  private val source = otag + key + ctag

  def render(
    context:Any, 
    partials:Map[String,Mustache], 
    callstack:List[Any]):TokenProduct = { 
      val v = format(valueOf(key,context,partials,callstack,"",defaultRender(otag,ctag)))
      new TokenProduct {
        val maxLength = (v.length*1.2).toInt
        def write(out:StringBuilder):Unit =
          v.foreach {
            case '<' => out.append("&lt;")
            case '>' => out.append("&gt;")
            case '&' => out.append("&amp;")
            case '"' => out.append("&quot;")
            case c => out.append(c)
          }
      }
  }

  def templateSource:String = source
}
