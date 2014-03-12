package flagship.console.terminal

import flagship.console.input.ConsoleKey
import flagship.console.widget.Window
import java.awt.event.{MouseEvent, MouseAdapter}


/**
 * User: mtrupkin
 * Date: 7/5/13
 */
class GUIConsole(val terminal: Terminal, val window: Window) {
  val framesPerSecond = 23
  val refreshRate = (1f / framesPerSecond) * 1000
  val screen = Screen(terminal.terminalSize)


  def completed(): Boolean = (window.closed || terminal.closed)

  var consoleKey: Option[ConsoleKey] = None

  val mouseAdapter = new MouseAdapter {
    override def mouseClicked(e: MouseEvent) {
      window.mouseClicked(terminal.mouse)
    }
  }

  terminal.addMouseAdapter(mouseAdapter)

  def render() {
    window.render(screen)
    terminal.flush(screen)
  }

  def processInput() {

    for (key <- terminal.key) {
      window.keyPressed(key)
      terminal.key = None
    }
    if (terminal.key != consoleKey) {
      consoleKey = terminal.key
      for (key <- consoleKey) {
        //window.keyPressed(key)
      }
    } else {
      for (key <- consoleKey) {
      }
    }
  }

  def doEventLoop() {
    var lastUpdateTime = System.currentTimeMillis()

    while (!completed()) {
      val currentTime = System.currentTimeMillis()
      val elapsedTime = currentTime - lastUpdateTime


      if (elapsedTime > refreshRate) {
        lastUpdateTime = currentTime
        processInput
        window.update(elapsedTime.toInt)
        render
      }
    }

    terminal.close()
  }
}
