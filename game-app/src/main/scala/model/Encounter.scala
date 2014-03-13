package model

import org.flagship.console.Point
import scala.util.Random
import flagship.console.terminal.Color

/**
 * Created by mtrupkin on 3/11/14.
 */
object Encounter {
  import org.flagship.console.PointImplicits._

  def createEncounter(ship: ShipPlan) {
    val pos = getRandomPosition(ship)
    val e = new BaseEntity("Spider", pos, char = 'S', color = Color.Red)
    ship.entities = e :: ship.entities
  }

  def getRandomPosition(ship: ShipPlan): (Int, Int) = {
    val (module, modulePos) = ShipUtils.randomModule(ship)
    val cell = ShipUtils.randomModuleCell(module)
    ship.shipCoords(modulePos, cell)

  }
}

object Rnd {
  val rnd = new Random

  def roll(max: Int): Int = {
    rnd.nextInt(max)
  }
}
