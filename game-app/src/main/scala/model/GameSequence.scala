package model

import org.flagship.console.Point
import org.newdawn.slick.util.pathfinding.AStarPathFinder
import util.TileBasedMapWrapper
import org.newdawn.slick.util.pathfinding.heuristics.ManhattanHeuristic
import org.trupkin.MainWindow

/**
 * Created by mtrupkin on 3/13/14.
 */
case class ShipStatus(name: String)
object ShipStatus {
  val Green = ShipStatus("Green")
  val Red = ShipStatus("Red")
  val Yellow = ShipStatus("Yellow")
}

class GameSequence(
  val player: Player,
  val ship: ShipPlan) {
  import PhaseStepType._
  import org.flagship.console.PointImplicits._

  var messages: List[String] = Nil

  def shipStatus: ShipStatus = {
    if ((ship.liveEntities.isEmpty) && (enemyShip == None)) ShipStatus.Green else ShipStatus.Red
  }

  def gameOverCondition = !player.alive || ship.destroyed

  var numPhases: Int = 7
  var phase: Int = numPhases
  var round: Int = 0
  var step: PhaseStepType = PlayerAction

  var enemyShip: Option[ShipPlan] = None


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
      } else {
        for (module <- ship(testPos).activation) {
          player.activate(this, module, testPos)
        }
      }

      if (player.movement < 0) {
        step = AllyAction
        player.resetMovement()
      }
    } else {
      println("move skipped")
    }
  }

  def endMovement() {
    if (step == PlayerAction) {
      step = AllyAction
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
      case EndPhase => if (gameOverCondition) {gameOver(); false } else { endPhase(); true }
      case PlayerAction => false
      case EnemyAction => if( freeEnemies.isEmpty ) { step = EndPhase; true } else false
    }
  }

  def beginEncounter() {
    ship.increaseShields(2)
    beginRound()
  }

  def endPhase() {
    player.resetMovement()
    step = PlayerAction

    phase -= 1
    if (phase <= 0) endRound()
  }

  def beginRound() {
    player.resetMovement()
    step = PlayerAction
    phase = numPhases
    round = round + 1
  }

  def endRound() {
    for(e <- enemyShip) {
      ship.attack(e)
      e.attack(ship)

      if (e.hull <= 0) {
        enemyShipDestroyed()
      }
    }

    ship.endRound()

    beginRound()
  }

  def enemyShipDestroyed() {
    addMessage("Enemy ship destroyed.")
    enemyShip = None
    player.bitcoins += 1
  }

  def gameOver() {
    step = EndGame
    addMessage("Game Over.  Press Enter to restart.")
  }

  def addMessage(message:String) {
    messages = message::messages
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
      } else if(ship.isOccupied(testPos.x, testPos.y)) {
        if (e.movement > 1) {
          e.movePosition(testPos)
        } else {
          e.endMovement()
        }
      } else {
        e.movePosition(testPos)
      }
    }
  }
}

object GameApp {
  def apply(): GameSequence = {
    val player = new Player
    val ship = ShipPlan.ship1(7, 7, 1, 1)

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
