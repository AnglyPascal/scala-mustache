package com.anglypascal.mustache

import com.rallyhealth.weejson.v1._

trait Obj2Scala {
  def val2Scala(obj: Value): Any = {
    obj match {
      case s: Str => s.str
      case n: Num => n.num
      case b: Bool => b.bool
      case s: Arr => s.arr.map(val2Scala).toSeq
      case o: Obj => o.obj.map(p => (p._1, val2Scala(p._2))).toMap
      case any => any
    }
  }
}
