package mustache 

import org.specs2.mutable._
import org.specs2.runner._

import com.rallyhealth.weejson.v1._

object Issue1Specification extends Specification {

  "mustache" should {

    "render values from previous context levels" >> {
      new Mustache(
"""{{#organization}}{{header}}
id: {{id}}
name: {{name}}
{{/organization}}"""
      ).render(Map(
        "header"->"Hello"
        ,"organization"->Map(
                          "id"->1
                          ,"name"->"My Organization"
                        )
      )).toString must be equalTo(
"""Hello
id: 1
name: My Organization
"""
      )
    }

    "render values from previous context levels with Value" >> {
      new Mustache(
"""{{#organization}}{{header}}
id: {{id}}
name: {{name}}
{{/organization}}"""
      ).render(Obj(
        "header"->"Hello",
        "organization"->
          Obj(
            "id"->1,
            "name"->"My Organization"
          )
      )).toString must be equalTo(
"""Hello
id: 1
name: My Organization
"""
      )
    }

  }

}

