package com.anglypascal.mustache 

import org.specs2.mutable._
import org.specs2.runner._

import com.rallyhealth.weejson.v1._
// import com.rallyhealth.weepickle.v1.WeePickle._

object DotNotationSpecification extends Specification {

  "mustache" should {

    "render {{.}} properly" >> {
      new Mustache(
        "{{#foo}}{{.}}{{/foo}}"
      ).render(Map(
        "foo" -> List(4,2)
      )).toString must be equalTo("42")
    }

    "render {{.}} properly with Value" >> {
      new Mustache(
        "{{#foo}}{{.}}{{/foo}}"
      ).render(Obj(
        "foo" -> Arr(Num(4), Num(2))
      )).toString must be equalTo("42")
    }

    "take map value '.' first when rendering {{.}}" >> {
      new Mustache(
        "{{#foo}}{{.}}{{/foo}}"
      ).render(Map(
        "foo" -> Map("."->"bar")
      )).toString must be equalTo("bar")
    }

    "take map value '.' first when rendering {{.}} with Value" >> {
      new Mustache(
        "{{#foo}}{{.}}{{/foo}}"
      ).render(Obj(
        "foo" -> Obj("."->"bar")
      )).toString must be equalTo("bar")
    }

  }

}

