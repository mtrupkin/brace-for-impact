package org.trupkin

import flagship.console.widget.{Border, Window}
import model.{Entity, ShipPlan, Player}
import flagship.console.control.Composite
import flagship.console.layout.{Layout, LayoutData, LayoutManager}
import org.flagship.console.{Point, Size}
import scala.Some
import view.{PlayerStatusView, ShipStatusView, MapView}
import flagship.console.input.ConsoleKey
import flagship.console.layout.LayoutData._
import org.flagship.console.Size
import scala.Some
import org.newdawn.slick.util.pathfinding.AStarPathFinder
import util.TileBasedMapWrapper
import org.newdawn.slick.util.pathfinding.heuristics.ManhattanHeuristic

/**
 * Created by mtrupkin on 3/8/14.
 */
class MainWindow(size: Size) extends Window(size, Some("Window")) {
  import org.flagship.console.PointImplicits._

  var time = 0

  val player = new Player
  val ship = ShipPlan.ship1
  val startPos = ship.startingPosition()
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
  val playerStatusPanel = new PlayerStatusView(player) with Border


  import LayoutData._

  detailPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)
  shipStatusPanel.controlLayout = Layout(right = GRAB)
  playerStatusPanel.controlLayout = Layout(bottom = GRAB, right = GRAB)

  detailPanel.addControl(shipStatusPanel)
  detailPanel.addControl(playerStatusPanel)

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

  val pathFinder = new AStarPathFinder(new TileBasedMapWrapper(ship), 100, false, new ManhattanHeuristic(1))

  def move(dir: Point) {
    val testPos = player.position.move(dir)
    if ( ship(testPos).move ) {
      ship.entity(testPos.x, testPos.y) match {
        case Some(e) => {e.attack(1); endTurn()}
        case None => {
          player.move(dir)
        }
      }

      if (player.movement < 0) {
        endTurn()
      }
    }
    if( ship(testPos).activation ) {
      ship(testPos).activate(ship, player)
      endTurn()
    }
  }

  def endTurn() {
    player.resetMovement()
    for(e <- ship.liveEntities()) {
      while (entityTurn(e)) {}
      e.resetMovement()
    }
  }
  def entityTurn(e: Entity): Boolean = {
    val path = pathFinder.findPath(null, e.position.x, e.position.y, player.position.x, player.position.y )

    if (( path != null) && (path.getLength > 0)) {
      val step = path.getStep(1)
      val testPos = Point(step.getX, step.getY)
      if (player.position == testPos) {
        player.attack(1)
        false
      } else {
        e.movePosition(Point(step.getX, step.getY))
        (e.movement > 0)
      }
    } else {
      false
    }
  }
}
