package view

import model.{Player, ShipPlan}
import flagship.console.control.Control
import flagship.console.terminal.Screen

/**
 * Created by mtrupkin on 3/12/14.
 */
class PlayerStatusView(val player: Player) extends Control {

    def render(screen: Screen) {
      screen.clear()
      screen.write(0, 0, "Commander")
      screen.write(0, 1, "Movement: " + player.movement)
      screen.write(0, 2, " Stamina: " + player.stamina)
    }
}