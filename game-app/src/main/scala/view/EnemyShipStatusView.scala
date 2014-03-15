package view

import model.{GameSequence, ShipPlan, TileMap}
import org.flagship.console.Size
import flagship.console.control.Control
import flagship.console.terminal.{Color, Screen}

/**
 * Created by mtrupkin on 3/11/14.
 */
class EnemyShipStatusView(val game: GameSequence) extends Control {
  import game._

  def render(screen: Screen) {

    screen.clear()
    screen.write(0, 0, "Enemy Ship")
    game.enemyShip match {
      case Some(ship) => {
        screen.write(0, 1, "Shields: " + ship.shields)
        screen.write(0, 2, "   Hull: " + ship.hull)
      }
      case None => screen.write(0, 1, "None")
    }



  }
}