package view

import flagship.console.control.Control
import flagship.console.terminal.{Color, Screen}
import model.{GameSequence, Entity, TileMap}
import org.flagship.console.{Point, Size}

/**
 * User: mtrupkin
 * Date: 12/17/13
 */
class MapView(val game: GameSequence, val viewPort: Size = Size(78, 31)) extends Control {
  val map = game.ship
  val player = game.player
  override def minSize: Size = viewPort
  //var origin = Point(0, 0)

  def render(screen: Screen) {
    for (
      y <- 0 until viewPort.height;
      x <- 0 until viewPort.width
    ) {
      val tile = map(x, y)
      screen.write(x, y, tile.char, tile.fg, tile.bg)
    }


    //map.entities().foreach( e => screen.write(e.position.x, e.position.y, e.char, e.color, Color.Black) )
    map.entities.filter(!_.alive).foreach( renderEntity(screen, _) )
    map.entities.filter(_.alive).foreach( renderEntity(screen, _) )

    renderEntity(screen, player)
    //screen.write(player.x, player.y, '@', Color.Yellow, Color.Black)
  }

  def renderEntity(screen: Screen, e: Entity) {
    screen.write(e.position.x, e.position.y, e.char, e.color, Color.Black)
  }
}
