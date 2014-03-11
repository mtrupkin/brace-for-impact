package flagship.console.terminal

/**
 * User: mtrupkin
 * Date: 7/5/13
 */
case class Color(r: Int, g: Int, b:Int)

object Color {
  val Black = Color(0, 0, 0)
  val White = Color(255, 255, 255)
  val LightGrey = Color(126, 126, 126)
  val Yellow = Color(255, 255, 0)
  val Blue = Color(0, 0, 255)
  val Red = Color(255, 0, 0)
  val Green = Color(0, 255, 0)
  val LightYellow = Color(126, 126, 0)
  val LightBlue = Color(0, 0, 126)
  val LightRed = Color(126, 0, 0)
  val LightGreen = Color(0, 126, 0)
}
