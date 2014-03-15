package model

import scala.Array._
import scala.Some
import org.flagship.console.Point

/**
 * Created by mtrupkin on 3/11/14.
 */
class ShipPlan(modules: Array[Array[Option[ModulePlan]]], var hull: Int, var shields: Int, var weapons: Int, var power: Int) extends TileMap {
  val maxHull = hull
  val maxShields = shields
  val maxWeapons = weapons
  val maxPower = power

  val numModuleX = 7
  val numModuleY = 5
  val moduleWidth = 11
  val moduleHeight = 6

  val widthInTiles = numModuleX * moduleWidth
  val heightInTiles = numModuleY * moduleHeight

  val bulkheadTile = new SimpleTile('x')
  val emptySpaceTile = new SimpleTile(' ')
  val airlockTile = new SimpleTile('X')
  val doorTile = new SimpleTile('.', true)
  val exteriorTile = new SimpleTile('#')

  def destroyed = hull <= 0

  def attack(other: ShipPlan) {
    other.damage(weapons)
  }

  def damage(amount: Int) {
    val remaining = shields - amount
    if (remaining < 0) {
      shields = 0
      hull += remaining
    } else {
      shields = remaining
    }
  }

  def increaseShields(amount: Int) {
    shields += amount
    if (shields > maxShields) shields = maxShields
  }

  def increasePower(amount: Int) {
    power += amount
  }

  def endRound() {
    for((m, p) <- getModules()) {
      m.module.status = ModuleStatus.Ready
    }
    power = maxPower
    weapons = reduceToMax(weapons, maxWeapons)
    shields = reduceToMax(shields, maxShields)
  }

  def reduceToMax(current: Int, max: Int): Int = {
    if (current > max) max else current
  }

  var entities : List[Entity] = List.empty
  def liveEntities: List[Entity] = entities.filter(_.alive)


  def startingPosition(): (Int, Int) = {
    val (m, pos) = getModule(ModuleType.Helm).filter(_._1.primary).head
    val cell = ShipUtils.randomModuleCell(m)
    shipCoords(pos, cell)
  }

  def getModules(): List[(ModulePlan, (Int, Int))] = {
    val m = for{
      i <- 0 until numModuleX
      j <- 0 until numModuleY
      module <- modules(i)(j)
    } yield (module, (i, j))

    m.toList
  }

  def getModule(module: ModuleType): List[(ModulePlan, (Int, Int))] = {
    val indices = for{
      (row, i) <- modules.iterator.zipWithIndex
      (col, j) <- row.iterator.zipWithIndex
      m <- col
      if ( (m.module.name == module.name) && (m.primary))
    } yield (m, (i, j))

    indices.toList
  }

  def apply(t: (Int, Int)): Tile = {
    apply(t._1, t._2)
  }

  def apply(x: Int, y: Int): Tile = {
    if ((x > 0) && (y > 0)) {
      toModule(x-1, y-1)
    } else {
      val ((mx, my), (dx, dy)) = moduleCoords(x-1, y-1)
      if (hasModule(x-1, y) || hasModule(x, y) || hasModule(x, y-1)) {
        getExteriorTile(dx, dy)
      } else {
        emptySpaceTile
      }
    }
  }

  def entity(x: Int, y: Int): Option[Entity] = {
    liveEntities.filter( _.position == Point(x, y)) match {
      case e::es => Some(e)
      case Nil => None
    }
  }

  def shipCoords(module: (Int, Int), cell: (Int, Int)): (Int, Int) = {
    val (mx, my) = module
    val (dx, dy) = cell
    (mx * moduleWidth + dx + 1, my * moduleHeight + dy + 1)
  }


  def moduleCoords(x: Int, y: Int): ((Int, Int), (Int, Int)) = {
    val (mx, my) = (x / moduleWidth, y / moduleHeight)
    val (dx, dy) = (x % moduleWidth, y % moduleHeight)
    ((mx, my), (dx, dy))
  }

  def toModule(x: Int, y: Int): Tile = {
    val ((mx, my), (dx, dy)) = moduleCoords(x, y)

    val tile: Tile = modules(mx)(my) match {
      case Some(module) => {
        module(dx, dy) match {
          case Some(x) => x
          case None => {
            //if (hasModule(x+1, y+1)) {
            if (hasModule(x+1, y) && hasModule(x, y+1) && hasModule(x+1, y+1) ) {
              getBulkheadTile(dx, dy)
            } else {
              getExteriorTile(dx, dy)
            }
          }
        }
      }
      case None => {
        if (hasModule(x+1, y) || hasModule(x, y+1)  || hasModule(x+1, y+1) ) {
          getExteriorTile(dx, dy)
        } else {
          emptySpaceTile
        }
      }
    }

    tile
  }

  def getExteriorTile(dx: Int, dy: Int): Tile = {
    if (isModuleCenter(dx, dy)) {
      airlockTile
    } else {
      exteriorTile
    }
  }

  def getBulkheadTile(dx: Int, dy: Int): Tile = {
    if (isModuleCenter(dx, dy)) {
      doorTile
    } else {
      bulkheadTile
    }
  }

  def isModuleCenter(dx: Int, dy: Int): Boolean = {
    if ( ((dx > 3) && (dx < 6)) || ((dy > 1) && (dy < 3)) ) {
      true
    } else {
      false
    }
  }

  def hasModule(x: Int, y: Int): Boolean = {
    val ((mx, my), (dx, dy)) = moduleCoords(x, y)
    if ((mx >= 0) && (my >= 0) &&(mx < numModuleX) && (my < numModuleY)) {
      modules(mx)(my) match {
        case Some(module) => true
        case None => false
      }
    } else {
      false
    }
  }

  def isOccupied(x: Int, y: Int): Boolean = {
    !liveEntities.filter(e => (e.position.x == x) && (e.position.y == y) ).isEmpty
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

object ShipPlan {
  def fromPlan(plan: String, hull: Int, shields: Int, weapons: Int, power: Int): ShipPlan = {
    val modules = ofDim[Option[ModulePlan]](7, 5)
    val lines = plan.lines.zipWithIndex

    for ((line, lineNum) <- lines) {
      var rowNum = 0
      for (c <- line) {
        modules(rowNum)(lineNum) = toModulePlan(c)
        rowNum  += 1
      }
    }

    new ShipPlan(modules, hull, shields, weapons, power)
  }

  def toModulePlan(c: Char): Option[ModulePlan] = {
    c.toUpper match {
      case 'H' => Some(ModulePlan.fromPlan(ModuleType.Helm, Character.isUpperCase(c), Modules.Helm0))
      case 'C' => Some(ModulePlan.fromPlan(ModuleType.Cargo, Character.isUpperCase(c), Modules.Cargo0))
      case 'D' => Some(ModulePlan.fromPlan(ModuleType.HyperDrive, Character.isUpperCase(c), Modules.HyperDrive0))
      case 'P' => Some(ModulePlan.fromPlan(ModuleType.PowerPlant, Character.isUpperCase(c), Modules.PowerPlant0))
      case 'T' => Some(ModulePlan.fromPlan(ModuleType.Transporter, Character.isUpperCase(c), Modules.Transporter0))
      case 'S' => Some(ModulePlan.fromPlan(ModuleType.Shields, Character.isUpperCase(c), Modules.Shields0))
      case 'W' => Some(ModulePlan.fromPlan(ModuleType.Phasers, Character.isUpperCase(c), Modules.Phasers0))

      case _ => None
    }
  }

  //val ship0 = fromPlan(ShipPlans.shipPlan0)
  def ship1(hull: Int, shields: Int, weapons: Int, power: Int) = fromPlan(ShipPlans.shipPlan1, hull, shields, weapons, power)
  def ship2(hull: Int, shields: Int, weapons: Int, power: Int) = fromPlan(ShipPlans.shipPlan2, hull, shields, weapons, power)
}

object ShipPlans {
  val shipPlan0 =
    """.......
      |..c....
      |..DPCH.
      |..c....
      |.......""".stripMargin

  val shipPlan1 =
    """.......
      |.dw....
      |..PscH.
      |.dw....
      |.......""".stripMargin

  val shipPlan1a =
    """.w.....
      |.ds....
      |..tPcH.
      |.ds....
      |.w.....""".stripMargin

  val shipPlan1b =
    """.......
      |..w....
      |.dsPcH.
      |..w....
      |.......""".stripMargin


  val shipPlan2 =
    """www.w.w
      |wH.....
      |w.....w
      |w......
      |ww..w.w""".stripMargin

  val shipPlan3 =
    """w.....c
      |.H.....
      |w.w....
      |...w...
      |....w..""".stripMargin
}