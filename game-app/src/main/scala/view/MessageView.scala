package view

import model.{GameSequence, ShipPlan}
import flagship.console.control.Control
import flagship.console.terminal.Screen

/**
 * Created by mtrupkin on 3/13/14.
 */
class MessageView(val gameSequence: GameSequence) extends Control {
  def render(screen: Screen) {

    screen.clear()

    for ((message, i) <- gameSequence.messages.take(dimension.height-2).reverse.zipWithIndex) {
      screen.write(0, i, message)
    }
  }
}
