# Mustache

## What is Mustache?

[Mustache][1] is a logic-free template engine inspired by ctemplate and et. 

As ctemplates says, "It emphasizes separating logic from presentation: it is impossible
to embed application logic in this template language".


## Usage

To get the language-agnostic overview of Mustache's template syntax and examples of
Mustache templates, see <http://mustache.github.io/mustache.5.html>. 

Getting started with scala-mustache is easy:
``` scala
import mustache._
...
val template = new Mustache("Hello, {{ name }}!")
template.render(Map("name"->"world"))
```
Returns following:
``` html
Hello, world!
```

### Partials

Following example shows how to use partials:
``` scala
val userTemplate = new Mustache("<strong>{{name}}</strong>")
val baseTemplate = new Mustache(
  "<h2>Names</h2>\n{{#names}}\n  {{> user}}\n{{/names}}"
)
val ctx = Map("names" -> List(
               Map("name"->"Alice"),
               Map("name"->"Bob")
            )
          )
val partials = Map("user" -> userTemplate)
baseTemplate.render(ctx, partials)
```
Templates defined here can be thought of as a single, expanded template:
``` mustache
<h2>Names</h2>
{{#names}}
  <strong>{{name}}</strong>
{{/names}}
```

You can use `{{.}}` and `{{{.}}}` to reference current context value:
``` scala
val template = new Mustache("{{#list}}{{.}} {{/list}}")
template.render(Map("list" -> List("alpha", "bravo", "charlie")))
```
Returns:
``` html
alpha bravo charlie 
```

### Rendering with objects

When the context value is a callable object, such as a function or lambda, the object
will be invoked and passed the block of text. The text passed is the literal block,
unrendered. `{{tags}}` will not have been expanded - the lambda should do that on its
own. In this way you can implement filters or caching.
``` scala
val template = new Mustache("{{#wrapped}}{{name}} is awesome.{{/wrapped}}")
template.render(Map(
    "name" -> "Willy",
    "wrapped" -> (
        (str: String, render: (String) => String) => { "<b>"+render(str)+"</b>" }
    )
)
```
Returns:
``` html
<b>Willy is awesome.</b>
```

Alternatively you can pack your helpers directly into the Mustache subclass. Following
example is effectively the same as previous:
``` scala
class MyMustache(template:String) 
  extends Mustache(template) {

  def wrapped(str:String) = "<b>"+render(str)+"</b>"

}
val template = new MyMustache("{{#wrapped}}{{name}} is awesome.{{/wrapped}}") 
template.render(Map("name"->"Willy"))
```

Sometimes it is nice to keep different kinds of helpers separate. To do so, you can
define helper traits and then mix them up as needed:

``` scala
trait MyHelper {
  this: MyHelper with MustacheHelperSupport =>

  def wrapped(str:String) = "<b>"+render(str)+"</b>"
}

class MyMustache(template:String) 
  extends Mustache(template) with MyHelper {}
```

MustacheHelperSupport trait defines following methods you can use in your helper methods:
``` scala
protected def context:Any                   // returns current context
protected def render(template:String):Any   // renders template string
```

### weePickle support

This library supports [weePickle][3] AST as context values. It also accepts Json strings as
input for context:

``` scala

val baseTemplate = new Mustache(
  "<h2>Names</h2>\n{{#names}}\n  {{> user}}\n{{/names}}"
)
val str = """{"names": [{"name": "Alice"}, {"name": "Bob"}]}"""
val ctx = Obj(
  "names"-> Arr(
    Obj("name"->Str("Alice")),
    Obj("name"->Str("Bob"))
  )
)
val partials = Map("user" -> userTemplate)

println(baseTemplate.render(obj, partials))
println(baseTemplate.render(str, partials))
```
Both returns
``` html
<h2>Names</h2>

  <strong>Alice</strong>

  <strong>Bob</strong>
```


## Dependencies / Build

This project has absolutely zero runtime dependencies and you can build it with [Simple Build Tool][2].


## Licensing

This project is licensed under the MIT license. 

I’m not a lawyer and this is not a legal advice, but it is free to use in any projects.
Free as in “free beer”. Should you have any questions on licensing, consult your
lawyer.

Enjoy !

[1]: https://mustache.github.io/
[2]: https://www.scala-sbt.org/
[3]: https://github.com/rallyhealth/weePickle/
