package org.trupkin

import flagship.console.widget.{Border, Window}
import model._
import flagship.console.control.Composite
import flagship.console.layout.{Layout, LayoutData, LayoutManager}
import org.flagship.console.Point
import view._
import flagship.console.input.ConsoleKey
import flagship.console.layout.LayoutData._
import org.flagship.console.Size
import scala.Some
import scala.Some
import org.flagship.console.Size

/**
 * Created by mtrupkin on 3/8/14.
 */
class MainWindow(size: Size) extends Window(size, Some("Window")) {
  var time = 0
  var game = model.GameApp()
  initialize()

  def initialize() {
    time = 0
    controls = Nil

    val mainPanel = new Composite(LayoutManager.VERTICAL)
    mainPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

    val topPanel = new Composite
    val messagePanel = new MessageView(game) with Border
    messagePanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

    val mapPanel = new MapView(game) with Border
    val detailPanel = new Composite(LayoutManager.VERTICAL)
    val shipStatusPanel = new ShipStatusView(game) with Border {
      override def minSize: Size = Size(40, 8)
    }
    val enemyShipPanel = new EnemyShipStatusView(game) with Border {
      override def minSize: Size = Size(40, 6)
    }
    val playerStatusPanel = new PlayerStatusView(game) with Border


    import LayoutData._

    detailPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
    shipStatusPanel.controlLayout = Layout(right = GRAB)
    enemyShipPanel.controlLayout = Layout(right = GRAB)
    playerStatusPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

    detailPanel.addControl(shipStatusPanel)
    detailPanel.addControl(enemyShipPanel)
    detailPanel.addControl(playerStatusPanel)

    topPanel.addControl(mapPanel)
    topPanel.addControl(detailPanel)

    mainPanel.addControl(topPanel)
    mainPanel.addControl(messagePanel)

    addControl(mainPanel)

    layout()
  }


  override def keyPressed(key: ConsoleKey) {
    import scala.swing.event.Key._

    val k = key.keyValue
    k match {
      case W | Up => game.move(Point.Up)
      case A | Left => game.move(Point.Left)
      case S | Down => game.move(Point.Down)
      case D | Right => game.move(Point.Right)
      case Space => game.endMovement()
      case Enter => accept()
      case Escape => closed = true
      case _ =>
    }
  }

  def accept() {
    if (game.step == PhaseStepType.EndGame) {
      game = model.GameApp()
      initialize()
    }
  }

  override def update(elapsedTime: Int) {
    time += elapsedTime

    game.update(elapsedTime: Int)
  }

}
