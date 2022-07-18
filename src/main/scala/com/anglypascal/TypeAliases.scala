package com.anglypascal.mustache

import scala.collection.Map

class TypeAliases {
  type Partials = Map[String, Mustache]
  type CallStack = List[Any]
  type Renderer = (Any, Partials, CallStack) => (String) => String
}
