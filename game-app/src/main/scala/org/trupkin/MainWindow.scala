package org.trupkin

import flagship.console.widget.{Border, Window}
import model.{ShipPlan, Player}
import flagship.console.control.Composite
import flagship.console.layout.{Layout, LayoutData, LayoutManager}
import org.flagship.console.{Point, Size}
import scala.Some
import view.{ShipStatusView, MapView}
import flagship.console.input.ConsoleKey
import flagship.console.layout.LayoutData._
import org.flagship.console.Size
import scala.Some

/**
 * Created by mtrupkin on 3/8/14.
 */
class MainWindow(size: Size) extends Window(size, Some("Window")) {
  import org.flagship.console.PointImplicits._

  var time = 0

  val player = new Player
  val ship = ShipPlan.ship1
  val startPos = ship.startingPosition().getOrElse(throw new IllegalStateException("starting position not found"))
  player.setPosition(startPos)

  val mainPanel = new Composite(LayoutManager.VERTICAL)
  mainPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

  val topPanel = new Composite
  val bottomPanel = new Composite with Border
  bottomPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

  val mapPanel = new MapView(ship, player.position) with Border
  val detailPanel = new Composite(LayoutManager.VERTICAL)
  val shipStatusPanel = new ShipStatusView(ship) with Border {
    override def minSize: Size = Size(40, 5)
  }
  val playerPanel = new Composite(LayoutManager.VERTICAL) with Border


  import LayoutData._

  detailPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  shipStatusPanel.controlLayout = Layout(right = GRAB)
  playerPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

  detailPanel.addControl(shipStatusPanel)
  detailPanel.addControl(playerPanel)

  topPanel.addControl(mapPanel)
  topPanel.addControl(detailPanel)

  mainPanel.addControl(topPanel)
  mainPanel.addControl(bottomPanel)

  addControl(mainPanel)

  layout()

  override def update(elapsedTime: Int) {
    time += elapsedTime
  }

  override def keyPressed(key: ConsoleKey) {
    import scala.swing.event.Key._
    val k = key.keyValue
    k match {
      case W | Up => move(Point.Up)
      case A | Left => move(Point.Left)
      case S | Down => move(Point.Down)
      case D | Right => move(Point.Right)
      case Escape => closed = true
      case _ =>
    }
  }

  def move(dir: Point) {
    val testPos = player.position.move(dir)
    if ( ship(testPos).move ) {
      val newPos = player.move(dir)
      ship(newPos).activate(ship, player)
    }
  }
}
