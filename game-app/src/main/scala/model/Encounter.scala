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
    val roll = Rnd.roll(4)
    roll match {
      case 0 => creaturesAttack(game)
      case 1 => shipAttacks(game)
      case 2 => {
        shipAttacks(game)
        creaturesAttack(game)
      }
      case 3 => {
        creaturesAttack(game, Rnd.roll(2)+1)
      }
    }
  }

  def Spider(pos: (Int, Int)) = new BaseEntity("Spider", pos, char = 'S', color = Color.Red)
  def Human(pos: (Int, Int)) = new BaseEntity("Human", pos, char = 'H', color = Color.Red)
  def Vrusk(pos: (Int, Int)) = new BaseEntity("Vrusk", pos, char = 'V', color = Color.Red)
  def Dralasites(pos: (Int, Int)) = new BaseEntity("Dralasites", pos, char = 'D', color = Color.Red)
  def Yazirian(pos: (Int, Int)) = new BaseEntity("Yazirian", pos, char = 'Y', color = Color.Red)

  def getRandomPosition(ship: ShipPlan): (Int, Int) = {
    val (module, modulePos) = ShipUtils.randomModule(ship)
    val cell = ShipUtils.randomModuleCell(module)
    ship.shipCoords(modulePos, cell)
  }

  def randomCreature(pos: (Int, Int)): Entity = {
    val roll = Rnd.roll(4)
    roll match {
      case 0 => Spider(pos)
      case 1 => Human(pos)
      case 2 => Vrusk(pos)
      case 3 => Dralasites(pos)
      case 4 => Yazirian(pos)
    }
  }

  def creaturesAttack(game: GameSequence, num: Int = 1) {
    game.addMessage("Boarding party detected.")
    val ship = game.ship
    for (i <- 0 until num) {
      val pos = getRandomPosition(ship)
      addEnitity(ship, randomCreature(pos))
    }
  }

  def addEnitity(ship:ShipPlan, e: Entity) {
    ship.entities = e :: ship.entities
  }

  def shipAttacks(game: GameSequence) {
    game.addMessage("Enemy ship detected.")

    val ship = game.ship
    val enemyShip = ShipPlan.ship2(2, 2, 1, 1)
    game.enemyShip = Option(enemyShip)
  }
}

object Rnd {
  val rnd = new Random

  def roll(max: Int): Int = {
    rnd.nextInt(max)
  }
}
