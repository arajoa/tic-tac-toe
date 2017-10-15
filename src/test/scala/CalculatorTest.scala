import org.scalatest.{FlatSpec, Matchers}

class CalculatorTest extends FlatSpec with Matchers {

  val sut: Calculator = new Calculator()

  "A Calculator" should "sum values" in {
    sut.sum(1, 2) shouldBe 3
  }

  it should "multiply values" in {
    sut.multiply(1, 2) shouldBe 2
  }
}
