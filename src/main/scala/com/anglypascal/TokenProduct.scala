package com.anglypascal.mustache 

/** A TokenProduct object represents a rendered token
 *  EmptyProduct represents failure
 *  StringProduct represents success
 */
trait TokenProduct {
  val maxLength: Int
  def write(out: StringBuilder): Unit

  override def toString = {
    val b = new StringBuilder(maxLength)
    write(b)
    b.toString
  }
}

object EmptyProduct extends TokenProduct {
  val maxLength = 0 
  def write(out: StringBuilder): Unit = {}
}

case class StringProduct(str: String) extends TokenProduct {
  val maxLength = str.length 
  def write(out: StringBuilder): Unit = out.append(str)
}
