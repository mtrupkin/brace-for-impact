package view

import model.{GameSequence, ShipPlan, TileMap}
import org.flagship.console.{Size, Point}
import flagship.console.control.Control
import flagship.console.terminal.{Color, Screen}

/**
 * Created by mtrupkin on 3/11/14.
 */
class ShipStatusView(val game: GameSequence) extends Control {
  import game._

  def render(screen: Screen) {

    val status = if ( game.ship.liveEntities.isEmpty) "Green" else "Red"

    screen.clear()
    screen.write(0, 0, "Starship")
    screen.write(0, 1, " Status: " + status)
    screen.write(0, 2, "Shields: " + ship.hull)
    screen.write(0, 3, "   Hull: " + ship.hull)
  }
}