package view

import model.{ShipPlan, TileMap}
import org.flagship.console.{Size, Point}
import flagship.console.control.Control
import flagship.console.terminal.{Color, Screen}

/**
 * Created by mtrupkin on 3/11/14.
 */
class ShipStatusView(val ship: ShipPlan) extends Control {

  def render(screen: Screen) {
    val status = if ( ship.entities.filter(_.alive).isEmpty) "Green" else "Red"

    screen.clear()
    screen.write(0, 0, "Status: " + status)
  }
}