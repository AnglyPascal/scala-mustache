package com.anglypascal.mustache 

import scala.collection.Map
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.concurrent.{Await, Awaitable}
import com.rallyhealth.weejson.v1._
import scala.concurrent.duration._

trait ContextHandler extends TypeAliases {

  protected def defaultRender(otag: String, ctag: String): Renderer = 
    (context: Any, partials: Partials, callstack: CallStack) => 
      (str: String) => {
        val t = new Mustache(str, otag, ctag)
        t.render(context, partials, callstack)
      }

  def valueOf(key: String, 
    context: Any, partials: Partials, callstack: CallStack, 
    childrenString: String, render: Renderer): Any = {
 
    val r = render(context, partials, callstack)

    // for all the Mustache object in callstack
    // fold the withContextAndRenderFn calls around the eval call
    val wrappedEval = callstack
      .filter(_.isInstanceOf[Mustache]).asInstanceOf[List[Mustache]]
      .foldLeft( () => 
        { eval(findInContext(context::callstack, key), childrenString, r) }
      )( (f, e) => {() => {e.withContextAndRenderFn(context, r)(f())}} )
    /** let [m1, m2, m3, ... mn] = callstack
     *  resulting fold:
     *  mn.wcrf(_) . ... . m1.wcrf(_) . eval(_)
     */

    wrappedEval() match {
      case None if (key == ".") => context
      case other => other
    }
  }

    
  @tailrec
  private def eval(value: Any, childrenString: String, render: (String) => String): Any =
    value match {
      case Some(someValue) => eval(someValue, childrenString, render)

      case s: Seq[_]   => s
      case m: Map[_,_] => m
      case s: Arr      => s.arr.toSeq
      case m: Obj      => m.obj.toMap

      // TODO: understand the synchronization part
      case a: Awaitable[_] => eval(Await.result(a, Duration.Inf), childrenString, render)

      case f: Function0[_] => eval(f(), childrenString, render)
      case f: Function1[String, _] => eval(f(childrenString), childrenString, render)
      case f: Function2[String, Function1[String, String], _] => 
        eval(f(childrenString, render), childrenString, render)

      case s: Str => s.str
      case n: Num => n.num
      case other  => other
    }


  // find the key in the callstack until
  // the key has been found
  // or the stack has been exhausted
  @tailrec
  private def findInContext(callstack: CallStack, key: String): Any =
    callstack.headOption match {
      case None => None
      case Some(head) =>
        (head match {
          case null => None
          case m: Map[String, _] =>
            m.get(key) match {
              case Some(v) => v
              case None => None
            }
          case m: Mustache =>
            m.globals.get(key) match {
              case Some(v) => v
              case None => None
            }
          case m: Obj => 
            m.obj.get(key) match {
              case Some(v) => v
              case None => None
            }
          case any => reflection(any, key)
        }) match {
          case None => findInContext(callstack.tail, key)
          case x => x
        }
    }

  // look for key inside the fields and methods of the wrapped object of x
  private def reflection(x: Any, key: String): Any = {
    val w = wrapped(x)
    (methods(w).get(key), fields(w).get(key)) match {
      case (Some(m), _)    => m.invoke(w)
      case (None, Some(f)) => f.get(w)
      case _               => None
    }
  }

  private def fields(w: AnyRef) = Map( 
    unsafeWrapArray(w.getClass().getFields.map(x => {x.getName -> x})):_*
  )

  private def methods(w: AnyRef) = Map(
    unsafeWrapArray(
      w.getClass()
      .getMethods
      .filter(x => { x.getParameterTypes.length == 0 })
      .map(x => { x.getName -> x })
    ) :_*
  )

  private def wrapped(x: Any): AnyRef =
    x match {
      case x: Byte    => byte2Byte(x)
      case x: Short   => short2Short(x)
      case x: Char    => char2Character(x)
      case x: Int     => int2Integer(x)
      case x: Long    => long2Long(x)
      case x: Float   => float2Float(x)
      case x: Double  => double2Double(x)
      case x: Boolean => boolean2Boolean(x)
      case _          => x.asInstanceOf[AnyRef]
    }
}
