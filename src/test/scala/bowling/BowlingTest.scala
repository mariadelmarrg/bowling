package bowling

import org.scalatest.{Matchers, WordSpec}

class BowlingTest extends WordSpec with Matchers {

  "The score of a bowling game" should {

    "be 90 for '9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||'" in {
      val input = "9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||"
      Bowling.calculateScore(input) shouldBe 90
    }

    "be 89 for '45|9-|9-|9-|9-|9-|9-|9-|9-|9-||'" in {
      val input = "45|9-|9-|9-|9-|9-|9-|9-|9-|9-||"
      Bowling.calculateScore(input) shouldBe ((4 + 5) + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9)
    }

    "be 91 for '9-|9-|9-|9-|9-|9-|9-|9-|9-|9/||--'" in {
      val input = "9-|9-|9-|9-|9-|9-|9-|9-|9-|9/||--"
      Bowling.calculateScore(input) shouldBe 91
    }

    "be 100 for '9/|9-|9-|9-|9-|9-|9-|9-|9-|9-||--'" in {
      val input = "9/|9-|9-|9-|9-|9-|9-|9-|9-|9-||--"
      Bowling.calculateScore(input) shouldBe ((9 + 1 + 9) + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9)
    }

    "be 100 for 'X|9-|9-|9-|9-|9-|9-|9-|9-|9-||--'" in {
      val input = "X|9-|9-|9-|9-|9-|9-|9-|9-|9-||--"
      Bowling.calculateScore(input) shouldBe ((10 + 9 + 0) + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9)
    }

    "be 111 for 'X|9/|9-|9-|9-|9-|9-|9-|9-|9-||--'" in {
      val input = "X|9/|9-|9-|9-|9-|9-|9-|9-|9-||--"
      Bowling.calculateScore(input) shouldBe ((10 + 9 + 1) + (9 + 1 + 9) + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9)
    }

    "be 111 for 'X|X|X|X|X|X|X|X|X|X||--'" in {
      val input = "X|X|X|X|X|X|X|X|X|X||--"
      Bowling.calculateScore(input) shouldBe ((10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 10) + (10 + 10 + 0) + (10 + 0 + 0))
    }

    "be 300 for 'X|X|X|X|X|X|X|X|X|X||XX'" in {
      val input = "X|X|X|X|X|X|X|X|X|X||XX"
      Bowling.calculateScore(input) shouldBe 300
    }

    "be 167 for 'X|7/|9-|X|-8|8/|-6|X|X|X||81'" in {
      val input = "X|7/|9-|X|-8|8/|-6|X|X|X||81"
      Bowling.calculateScore(input) shouldBe 167
    }

    "be 167 for 'X|7/|X|X|-8|8/|-6|X|X|X||81'" in {
      val input = "X|7/|X|X|-8|8/|-6|X|X|X||81"
      Bowling.calculateScore(input) shouldBe ((10+7+3) + (7 + 3 + 10) + (10 + 10 + 0) + (10 + 0 + 8) + 8 + (8 + 2 + 0) + 6 + (10 + 10 + 10) + (10 + 10 + 8) + (10 + 8 + 1))
    }

    "be 0 for '--|--|--|--|--|--|--|--|--|--||'" in {
      val input = "--|--|--|--|--|--|--|--|--|--||"
      Bowling.calculateScore(input) shouldBe 0
    }

  }

}
