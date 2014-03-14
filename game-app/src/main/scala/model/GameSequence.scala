package model

import org.flagship.console.Point
import org.newdawn.slick.util.pathfinding.AStarPathFinder
import util.TileBasedMapWrapper
import org.newdawn.slick.util.pathfinding.heuristics.ManhattanHeuristic
import org.trupkin.MainWindow

/**
 * Created by mtrupkin on 3/13/14.
 */
class GameSequence(
  val player: Player,
  val ship: ShipPlan) {
  import PhaseStepType._
  import org.flagship.console.PointImplicits._

  def gameOver = !player.alive || ship.destroyed

  var phase: Int = 0
  var step: PhaseStepType = PlayerAction

  def enemies: List[Entity] = ship.liveEntities
  def freeEnemies: List[Entity]  = enemies.filter(p => p.movement > 0)
  val pathFinder = new AStarPathFinder(new TileBasedMapWrapper(ship), 100, false, new ManhattanHeuristic(1))

  def move(dir: Point) {
    if (step == PlayerAction) {
      val testPos = player.position.move(dir)

      if ( ship(testPos).move ) {
        ship.entity(testPos.x, testPos.y) match {
          case Some(e) => player.attack(e)
          case None => player.move(dir)
        }
      } else if( ship(testPos).activation ) {
        player.activate(ship, testPos)

      }

      if (player.movement < 0) {
        player.resetMovement()
        step = AllyAction
      }
    }
  }

  def update(elapsed: Int) {
    step match {
      case PlayerAction =>
      case EnemyAction => enemiesStep()
      case EndGame =>
      case _ => while(autoSteps()) {}
    }
  }

  def autoSteps(): Boolean = {
    step match {
      case AllyAction => step = EnemyAction; true
      case EndPhase => if (gameOver) {step = EndGame; false } else { step = PlayerAction; true }
      case PlayerAction => false
      case EnemyAction => if( freeEnemies.isEmpty ) { step = EndPhase; true } else false
    }
  }

  def enemiesStep() {
    freeEnemies match {
      case e::es => enemyAction(e)
      case Nil => {
        enemies.foreach(_.resetMovement())
        step = EndPhase
      }
    }
  }


  def enemyAction(e: Entity) {
    val path = pathFinder.findPath(null, e.position.x, e.position.y, player.position.x, player.position.y )

    if (( path != null) && (path.getLength > 0)) {
      val step = path.getStep(1)
      val testPos = Point(step.getX, step.getY)
      if (player.position == testPos) {
        e.attack(player)
      } else {
        e.movePosition(Point(step.getX, step.getY))
      }
    }
  }
}

object GameApp {
  def apply(): GameSequence = {
    val player = new Player
    val ship = ShipPlan.ship1

    val (x, y) = ship.startingPosition()
    player.setPosition(Point(x, y))

    new GameSequence(player, ship)
  }
}

case class PhaseStepType(name: String)

object PhaseStepType {
  val PlayerAction = PhaseStepType("PlayerAction")
  val AllyAction = PhaseStepType("AllyAction")
  val EnemyAction = PhaseStepType("EnemyAction")
  val EndPhase = PhaseStepType("EndPhase")
  val EndGame = PhaseStepType("EndGame")
}
