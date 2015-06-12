package bowling

import bowling.Frame._

case class Frame(firstBall: Int, secondBall: Option[Int] = None, isBonus: Boolean = false) {

  def score = firstBall + secondBall.getOrElse(0)

  def isStrike = firstBall == MAX_POINTS_FRAME && secondBall.isEmpty

  def isSpare = secondBall.isDefined && score == MAX_POINTS_FRAME
}

object Frame {
  val MAX_POINTS_FRAME = 10
}

object Bowling {

  def calculateScore(input: String): Int = {
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
        Frame(MAX_POINTS_FRAME, None)
      case TWO_STRIKES_IN_BONUS =>
        Frame(MAX_POINTS_FRAME, Some(MAX_POINTS_FRAME))
      case MISS_TWO_BALLS =>
        Frame(0, Some(0))
      case SPARE(firstBall, secondBall) =>
        val firstBallPins = toInt(firstBall, 0)
        val secondBallPins = MAX_POINTS_FRAME - firstBallPins
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
      case f :: Nil if f.isBonus =>
        0
      case f :: Nil =>
        f.score
      case f1 :: f2 :: fs if f1.isSpare =>
        f1.score + f2.firstBall + calculate(frames.tail)
      case f1 :: (f2@Frame(_, Some(_), _)) :: fs if f1.isStrike =>
        f1.score + f2.score + calculate(frames.tail)
      case f1 :: (f2@Frame(_, None, _)) :: f3 :: fs if f1.isStrike =>
        f1.score + f2.firstBall + f3.firstBall + calculate(frames.tail)
      case f1 :: fs =>
        f1.score + calculate(frames.tail)
    }
  }

}
