package bowling

case class Frame(firstBall: Int, secondBall: Option[Int] = None, isSpare: Boolean = false, isStrike: Boolean = false, isBonus: Boolean = false) {
  def score = firstBall + secondBall.getOrElse(0)
}

object Bowling {

  def calculateScore(input: String) : Int = {
    val normalAndBonusFrames: Array[String] = input.split("\\|\\|")
    val normalFrames = normalAndBonusFrames(0).split('|')
    val bonusFrame = if (normalAndBonusFrames.length == 2) Seq(buildFrame(normalAndBonusFrames(1)).copy(isBonus = true)) else Seq()
    val frames = normalFrames.map(buildFrame) ++ bonusFrame
    calculate(frames.toList)
  }

  private val missSecondBall = "([1-9]\\-)".r
  private val missFirstBall = "(\\-[1-9])".r
  private val twoBalls = "([1-9])([1-9])".r
  private val spare = "([1-9])(\\/)".r
  private val strike = "X"
  private val twoStrikesInBonus = "XX"
  private val missTwoBalls = "--"

  private def buildFrame(frame: String): Frame = {

    def toInt(value: String, pos: Int) = Integer.parseInt(value.charAt(pos).toString)

    frame match {
      case `strike` =>
        Frame(10, None, isStrike = true)
      case `twoStrikesInBonus` =>
        Frame(10, Some(10))
      case `missTwoBalls` =>
        Frame(0, Some(0))
      case spare(firstBall, secondBall) =>
        val firstBallPins = toInt(firstBall, 0)
        val secondBallPins = 10 - firstBallPins
        Frame(firstBallPins, Some(secondBallPins), isSpare = true)
      case missSecondBall(value) =>
        Frame(toInt(value, 0), Some(0))
      case missFirstBall(value) =>
        Frame(0, Some(toInt(value, 1)))
      case twoBalls(firstBall, secondBall) =>
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
