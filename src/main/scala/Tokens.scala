package mustache 

import scala.collection.Map
import com.rallyhealth.weejson.v1._

/** RootToken to present that whole rendering job
 */
case class RootToken(children: List[Token]) 
  extends Token with CompositeToken {
  private val childrenSource = children.map(_.templateSource).mkString

  def render(context: Any, partials: Partials, callstack: CallStack): TokenProduct =
      composite(children, context, partials, callstack)

  def templateSource: String = childrenSource
}

/** Token that failed to render
 */
case class IncompleteSection(
  key: String, 
  inverted: Boolean, 
  otag: String, ctag: String) extends Token {

  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct = fail

  def templateSource: String = fail

  private def fail = 
    throw new Exception("Weird thing happened. " + 
      "There is incomplete section in compiled template.")
}

/** static Token that has nothing to render
 */
case class StaticTextToken(staticText: String) 
  extends Token {
  private val product = StringProduct(staticText)

  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct = product

  def templateSource: String = staticText
}
 
/** changes delimiters, does nothing else
 */
case class ChangeDelimitersToken(
  newOTag: String, newCTag: String, 
  otag: String, ctag: String) extends Token {

  private val source = otag + "=" + newOTag + " " + newCTag + "=" + ctag

  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct = EmptyProduct 

  def templateSource: String = source
}

/** PartialToken represents the partial mustache template
 *  which expands at the {{>key}} tag 
 *  _.render() returns the TokenProduct with the partial expansion
 */
case class PartialToken(key: String, otag: String, ctag: String) 
  extends Token {

  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct =
      partials.get(key) match {
        case Some(template) => 
          template.product(context, partials, template::callstack)
        case _ => 
          throw new IllegalArgumentException("Partial \""+ key +"\" is not defined.")
      }
  def templateSource:String = otag+">"+key+ctag
}

/** SectionToken represents the token for a section of mustache template
 *  starting and ending at the tag key
 *  Also takes a list of children Token, which it composes after successful rendering
 */
case class SectionToken(
  inverted: Boolean,
  key: String,
  children: List[Token],
  startOTag: String, startCTag: String,
  endOTag: String, endCTag: String) 
  extends Token 
  with ContextHandler with CompositeToken {

  private val childrenSource = 
    children.map(_.templateSource).mkString

  private val source = // source template string
    startOTag + (if(inverted) "^" else "#") + key + startCTag +  
      childrenSource + 
    endOTag + "/" + key + endCTag

  private val childrenTemplate = 
    new Mustache(
      if(children.size == 1) 
        children(0)
      else 
        RootToken(children)
    )

  // render this Token first, 
  // then compose with children where appropriate 
  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct =
      valueOf(key, context, partials, callstack, childrenSource, renderContent) 
        match {
          case null => 
            if (inverted) 
              composite(children, context, partials, context::callstack)
            else EmptyProduct

          case None => 
            if (inverted) 
              composite(children, context, partials, context::callstack)
            else EmptyProduct

          case b: Boolean => 
            if (b^inverted) 
              composite(children, context, partials, context::callstack)
            else EmptyProduct

          case b: Bool => 
            if (b.bool^inverted) 
              composite(children, context, partials, context::callstack)
            else EmptyProduct

          case s: Seq[_] if(inverted) => 
            if (s.isEmpty) 
              composite(children, context, partials, context::callstack)
            else EmptyProduct

          // For each element of s, make a copy of each child 
          // and render them in sequence
          case s: Seq[_] if(!inverted) => {
            val tasks = for (
              element <- s;
              token <- children
            ) yield (token, element)
            composite(tasks, partials, context::callstack)
          }

          // if this needs to be replaced by a string, 
          // there can't be any chindren token
          case str: String => 
            if (!inverted) 
              StringProduct(str)
            else EmptyProduct

          case str: Str => 
            if (!inverted) 
              StringProduct(str.str)
            else EmptyProduct

          // in case of wrapped value returned by valueOf
          // render with that value before continuing
          case other => 
            if (!inverted) 
              composite(children, other, partials, context::callstack)
            else EmptyProduct
        }

  private def renderContent(
    context: Any, 
    partials: Partials, 
    callstack: CallStack) (template: String): String =
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

case class UnescapedToken(key: String, otag: String, ctag: String) 
  extends Token with ContextHandler with ValuesFormatter {

  private val source = otag + "&" + key + ctag

  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct = {
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

  def render(context: Any, 
    partials: Partials, 
    callstack: CallStack): TokenProduct = { 
      val value = valueOf(key,context,partials,callstack,"",defaultRender(otag,ctag))
      val v = format(value)
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
