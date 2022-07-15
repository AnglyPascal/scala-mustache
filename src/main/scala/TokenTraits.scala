package mustache 

import scala.collection.Map

/** Tokens hold chunks of the template 
 *  provides render method 
 */
trait Token extends TypeAliases {
  def render(context: Any, partials: Partials, callstack: CallStack): TokenProduct
  def templateSource:String
}

/** CompositeToken combines a list of tokens
 */
trait CompositeToken extends TypeAliases {
  def composite(
    tokens: List[Token], 
    context: Any, partials: Partials, callstack: CallStack): TokenProduct = 
      composite(tokens.map( (_, context) ), partials, callstack)

  // renders all the tokens passed in, and combines their output
  def composite(
    tasks: Seq[Tuple2[Token, Any]], 
    partials: Partials, callstack: CallStack): TokenProduct = {

    val result = tasks.map(t => { t._1.render(t._2, partials, callstack) })
    val len = result.foldLeft(0)({ _ + _.maxLength })

    new TokenProduct {
      val maxLength = len
      def write(out: StringBuilder) = result.map( _.write(out) )
    }
  }
}

