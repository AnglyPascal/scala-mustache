import org.specs2.mutable._
import org.specs2.runner._


package mustache {
object LambdaSpecification extends Specification {

  "mustache" should {

    "render values returned by no-args functions" >> {
      new Mustache(
        "{{value}}"
      ).render(Map(
        "value"->(()=>{"hey!"})
      )).toString must be equalTo("hey!")
    }

    "render values returned by functions with string arg" >> {
      new Mustache(
        "{{#bold}}some text{{/bold}}"
      ).render(Map(
        "bold"->((str:String)=>{ "<b>"+str+"</b>" })
      )).toString must be equalTo("<b>some text</b>")
    }

    "render values returned by functions with render param" >> {
      new Mustache(
        "{{#bold}}Hello, {{name}}!{{/bold}}"
      ).render(Map(
        "name"-> "world"
        ,"bold"->((str:String, render:(String)=>String)=>{ "<b>"+render(str)+"</b>" })
      )).toString must be equalTo("<b>Hello, world!</b>")
    }

    "correctly remember open and close tags when rendering dynamic templates" >> {
      new Mustache(
        "{{= ** ** =}}**#bold**Hello,**=< >=** <name>!<=__ __=>__/bold__"
      ).render(Map(
        "name"-> "world"
        ,"bold"->((str:String, render:(String)=>String)=>{ "<b>"+render(" "+str+" ")+"</b>" })
      )).toString must be equalTo("<b> Hello, world! </b>")
    }

  }

}
}

