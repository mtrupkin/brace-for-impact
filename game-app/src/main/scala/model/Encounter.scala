package model

import org.flagship.console.Point
import scala.util.Random
import flagship.console.terminal.Color

/**
 * Created by mtrupkin on 3/11/14.
 */
object Encounter {
  import org.flagship.console.PointImplicits._

  def createEncounter(game: GameSequence) {
    val roll = Rnd.roll(2)
    roll match {
      case 0 => spidersAttack(game)
      case 1 => shipAttacks(game)
    }
  }


  def getRandomPosition(ship: ShipPlan): (Int, Int) = {
    val (module, modulePos) = ShipUtils.randomModule(ship)
    val cell = ShipUtils.randomModuleCell(module)
    ship.shipCoords(modulePos, cell)
  }

  def spidersAttack(game: GameSequence) {
    game.addMessage("New Encounter: Spiders Attack")

    val ship = game.ship
    val pos = getRandomPosition(ship)
    val e = new BaseEntity("Spider", pos, char = 'S', color = Color.Red)
    ship.entities = e :: ship.entities
  }

  def shipAttacks(game: GameSequence) {
    game.addMessage("New Encounter: Ship Attack")

    val ship = game.ship
    val enemyShip = ShipPlan.ship2
    game.enemyShip = Option(enemyShip)

  }
}

object Rnd {
  val rnd = new Random

  def roll(max: Int): Int = {
    rnd.nextInt(max)
  }
}
