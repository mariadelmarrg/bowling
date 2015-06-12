package bowling

case class Frame(firstBall: Int, secondBall: Option[Int] = None, isBonus: Boolean = false) {
  def score = firstBall + secondBall.getOrElse(0)
  def isStrike = firstBall == 10 && secondBall.isEmpty
  def isSpare = secondBall.isDefined && score == 10
}

object Bowling {

  def calculateScore(input: String) : Int = {
    val normalAndBonusFrames: Array[String] = input.split("\\|\\|")
    val normalFrames = normalAndBonusFrames(0).split('|')
    val bonusFrame = if (normalAndBonusFrames.length == 2) Seq(buildFrame(normalAndBonusFrames(1)).copy(isBonus = true)) else Seq()
    val frames = normalFrames.map(buildFrame) ++ bonusFrame
    calculate(frames.toList)
  }

  private val MISS_SECOND_BALL = "([1-9]\\-)".r
  private val MISS_FIRST_BALL = "(\\-[1-9])".r
  private val TWO_BALLS = "([1-9])([1-9])".r
  private val SPARE = "([1-9])(\\/)".r
  private val STRIKE = "X"
  private val TWO_STRIKES_IN_BONUS = "XX"
  private val MISS_TWO_BALLS = "--"

  private def buildFrame(frame: String): Frame = {

    def toInt(value: String, pos: Int) = Integer.parseInt(value.charAt(pos).toString)

    frame match {
      case STRIKE =>
        Frame(10, None)
      case TWO_STRIKES_IN_BONUS =>
        Frame(10, Some(10))
      case MISS_TWO_BALLS =>
        Frame(0, Some(0))
      case SPARE(firstBall, secondBall) =>
        val firstBallPins = toInt(firstBall, 0)
        val secondBallPins = 10 - firstBallPins
        Frame(firstBallPins, Some(secondBallPins))
      case MISS_SECOND_BALL(value) =>
        Frame(toInt(value, 0), Some(0))
      case MISS_FIRST_BALL(value) =>
        Frame(0, Some(toInt(value, 1)))
      case TWO_BALLS(firstBall, secondBall) =>
        Frame(firstBall.toInt, Some(secondBall.toInt))
    }
  }

  private def calculate(frames: List[Frame]): Int = {
    frames match {
      case frame:: Nil if frame.isBonus =>
        0
      case frame :: Nil =>
        frame.score
      case frame1 :: frame2 :: otherFrames if frame1.isSpare =>
        frame1.score + frame2.firstBall + calculate(frame2 :: otherFrames)
      case frame1 :: frame2 :: otherFrames if frame1.isStrike && frame2.secondBall.isDefined =>
        frame1.score + frame2.score + calculate(frame2 :: otherFrames)
      case frame1 :: frame2 :: frame3 :: otherFrames if frame1.isStrike && frame2.secondBall.isEmpty =>
        frame1.score + frame2.firstBall + frame3.firstBall + calculate(frame2 :: frame3 :: otherFrames)
      case frame1 :: otherFrames =>
        frame1.score + calculate(otherFrames)
    }
  }

}
