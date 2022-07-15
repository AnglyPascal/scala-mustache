import org.specs2.mutable._
import org.specs2.runner._

package mustache {

object ParserSpecification extends Specification {

  "parser" should {

    "handle static text only" >> {
      new Mustache(
        "Hello, world!"
      ).render().toString must be equalTo(
        "Hello, world!"
      )
    }

    "handle simple values" >> {
      new Mustache(
        "Hello, {{name}}!"
      ).render(
        Map("name"->"world")
      ).toString must be equalTo(
        "Hello, world!"
      )
    }
  
    "handle escaping properly" >> {
      new Mustache(
        "Hello, {{name}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, &lt;tag&gt;!"
      )
    }

    "handle unescaped values" >> {
      new Mustache(
        "Hello, {{{name}}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, <tag>!"
      )
    }

    "handle unescaped tags with &" >> {
      new Mustache(
        "Hello, {{&name}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, <tag>!"
      )
    }

    "report error for unbalanced braces >> {{{ }}}" >> {
      // unbalanced { inside the tag
      new Mustache("Hello, {{{name}}!") must throwA[MustacheParseException]
    }

    "ignore incomplete tags" >> {
      new Mustache(
        "{ { {"
      ).render().toString must be equalTo(
        "{ { {"
      )
      new Mustache(
        "} }} }"
      ).render().toString must be equalTo(
        "} }} }"
      )
    }

    "report error for empty tag" >> {
      new Mustache("{{{}}}") must throwA[MustacheParseException]
      new Mustache("{{}}") must throwA[MustacheParseException]
    }

    "handle sections" >> {
      new Mustache(
        "Message: {{#needToGreet}}Hello, {{name}}!{{/needToGreet}}"
      ).render(
        Map("needToGreet"->true, "name"->"world")
      ).toString must be equalTo(
        "Message: Hello, world!"
      )

      new Mustache(
        "Message: {{#needToGreet}}Hello, {{name}}!{{/needToGreet}}"
      ).render(
        Map("needToGreet"->false, "name"->"world")
      ).toString must be equalTo(
        "Message: "
      )
    }

    "handle nested sections" >> {
      new Mustache(
        "{{#foo}}>>{{#bar}}Hello, {{name}}!{{/bar}}<<{{/foo}}"
      ).render(
        Map("foo"->Map("bar"->Map("name"->"world")))
      ).toString must be equalTo(
        ">>Hello, world!<<"
      )
    }

    "report error for unclosed section" >> {
      new Mustache("some text {{#foo}} some internal text") must throwA[MustacheParseException]
    }

    "report error for unclosed tag" >> {
      new Mustache("some text {{unclosed tag") must throwA[MustacheParseException]
    }

    "report error for messed up sections" >> {
      new Mustache("text {{#foo}} {{#bar}} txt {{/foo}} {{/bar}}") must throwA[MustacheParseException]
    }

    "report error for invalid delimiter tag" >> {
      new Mustache("some text {{=}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{==}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{= foo =}} some text") must throwA[MustacheParseException]
    }

    "report error for invalid tags" >> {
      new Mustache("some text {{>}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{<}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{&}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{^}}...{{/}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{#}}...{{/}} some text") must throwA[MustacheParseException]
    }

    "report lines properly" >> {
      new Mustache(
        "some text\nand some more\n{{>}}\nsome text again"
      ) must throwA(MustacheParseException(3, "Empty tag"))
    }

  }

}

}
