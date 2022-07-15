import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object StaticTextTokenSpecification extends Specification {

  object SampleTemplate extends Mustache("")

  "static text token" should {
    "render static text" >> {
      StaticTextToken("Hey!").render(
        null, Map(), List(SampleTemplate)
      ).toString must be equalTo("Hey!")
    }
  }

}
}
