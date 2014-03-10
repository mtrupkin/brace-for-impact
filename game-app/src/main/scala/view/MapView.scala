package view

import flagship.console.control.Control
import flagship.console.terminal.Screen
import model.TileMap
import org.flagship.console.{Point, Size}

/**
 * User: mtrupkin
 * Date: 12/17/13
 */
class MapView(val map: TileMap, origin: => Point, val viewPort: Size = Size(78, 31)) extends Control {
  override def minSize: Size = viewPort
  //var origin = Point(0, 0)

  def render(screen: Screen) {
    val w2 = viewPort.width/2
    val h2 = viewPort.height/2
    for (
      y <- 0 until viewPort.height;
      x <- 0 until viewPort.width
    ) {
      //screen.write(x, y, map(x + origin.x - w2, y  + origin.y - h2).char)
      screen.write(x, y, map(x, y).char)
    }
  }

}
