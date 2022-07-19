package com.anglypascal.mustache 

import org.specs2.mutable._
import org.specs2.runner._

import com.rallyhealth.weejson.v1._

object EscapedTokenSpecification extends Specification {

  object SampleTemplate extends Mustache("")

  "escaped text token" should {
    "render escaped text" >> {
      EscapedToken("foo","{{","}}").render(
        Map("foo"->"\"<>&test\""), Map(), List(SampleTemplate)
      ).toString must be equalTo("&quot;&lt;&gt;&amp;test&quot;")
    }
  }
}

