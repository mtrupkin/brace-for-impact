package view

import model.{GameSequence, ShipPlan}
import flagship.console.control.Control
import flagship.console.terminal.Screen

/**
 * Created by mtrupkin on 3/13/14.
 */
class MessageView(val gameSequence: GameSequence) extends Control {
  def render(screen: Screen) {
    if ( gameSequence.gameOver ) {
      screen.clear()
      screen.write(0, 0, "Game Over")
      screen.write(0, 1, "Press Enter to Restart")
    }
  }
}
