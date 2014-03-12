package model

import org.flagship.console.Point
import scala.util.Random

/**
 * Created by mtrupkin on 3/11/14.
 */
object Encounter {
  import org.flagship.console.PointImplicits._

  def createEncounter(ship: ShipPlan) {
    val pos = getRandomPosition(ship)
    val e = new BaseEntity("Spider", pos)
    ship.entities = e :: ship.entities
  }

  def getRandomPosition(ship: ShipPlan): (Int, Int) = {
    val (module, modulePos) = ShipUtils.randomModule(ship)
    val cell = ShipUtils.randomModuleCell(module)
    ship.shipCoords(modulePos, cell)

  }
}

object ShipUtils {
  def randomModule(ship: ShipPlan): (ModulePlan, (Int, Int)) = {
    val modules = ship.getModules()
    val n = Rnd.roll(modules.size)
    modules(n)
  }

  def randomModuleCell(module: ModulePlan): (Int, Int) = {
    val cells = module.getCells().filter( p=> p._1.move)

    val n = Rnd.roll(cells.size)
    val cell = cells(n)
    cell._2
  }
}

object Rnd {
  val rnd = new Random

  def roll(max: Int): Int = {
    rnd.nextInt(max)
  }
}
