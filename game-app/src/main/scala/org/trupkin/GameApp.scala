package org.trupkin

/**
 * User: mtrupkin
 * Date: 11/29/13
 */

import scala.Some
import flagship.console.widget.Window
import flagship.console.terminal.{GUIConsole, SwingTerminal}
import org.flagship.console.Size


// TODO: fix right grab for vertical layout
object GameApp extends App {
  val size = Size(120, 42)
  val term = new SwingTerminal(size, "Rama Terminal")
  val window = new MainWindow(size)

  val gui = new GUIConsole(term, window)

  gui.doEventLoop()
}