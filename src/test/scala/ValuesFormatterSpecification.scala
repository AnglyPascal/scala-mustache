package com.anglypascal.mustache 

import org.specs2.mutable._
import org.specs2.runner._

object ValuesFormatterSpecification extends Specification {

  "values formatter" should {
    object T extends ValuesFormatter

    "render empty string for null and none" >> {
      T.format(null) must be equalTo("")
      T.format(None) must be equalTo("")
    }
    "render options properly" >> {
      T.format(Some("abc")) must be equalTo("abc")
      T.format(Some(Some("abc"))) must be equalTo("abc")
    }
    "render integers" >> {
      T.format(42) must be equalTo("42")
    }
  }
}
