package org.trupkin

import org.flagship.console.Size
import flagship.console.widget.{Border, Window}
import model.{TestMap, Player}
import flagship.console.control.Composite
import flagship.console.layout.{Layout, LayoutData, LayoutManager}
import flagship.console.layout.LayoutData._
import org.flagship.console.Size
import scala.Some
import view.MapView
import flagship.console.input.ConsoleKey

/**
 * Created by mtrupkin on 3/8/14.
 */
class MainWindow(size: Size) extends Window(size, Some("Window")) {
  var time = 0

  val player = new Player

  val windowPanel = new Composite()
  val mainPanel = new Window(Size(70, 40)) with Border
  val mapPanel = new Composite(LayoutManager.VERTICAL) with Border
  val insideMapPanel = new Composite() with Border
  val outsideMapPanel = new Composite() with Border
  val detailPanel = new Composite(LayoutManager.VERTICAL) with Border

  import LayoutData._

  windowPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  //mainPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  mapPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  insideMapPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  outsideMapPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  detailPanel.controlLayout = Layout(bottom = GRAB, right = SNAP)

  mainPanel.addControl(new MapView(new TestMap, player.position) with Border)

  mapPanel.addControl(insideMapPanel)
  mapPanel.addControl(outsideMapPanel)

  windowPanel.addControl(mainPanel)
  windowPanel.addControl(mapPanel)
  windowPanel.addControl(detailPanel)


  addControl(windowPanel)
  layout()

  override def update(elapsedTime: Int) {
    time += elapsedTime
  }

  override def keyPressed(key: ConsoleKey) {
    import scala.swing.event.Key._
    val k = key.keyValue
    k match {
      case W => player.up
      case A => player.left
      case S => player.down
      case D => player.right
      case _ =>
    }
  }
}
