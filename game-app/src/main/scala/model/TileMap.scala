package model

import org.flagship.console.{Point, Size}
import Array._
import flagship.console.terminal.Color

/**
 * User: mtrupkin
 * Date: 12/7/13
 */

//type TileArray = Array[Array[Tile]]

trait TileMap {
  def apply(x: Int, y: Int): Tile
}

trait Tile {
  def char: Char
  def fg: Color
  def bg: Color
}

class SimpleTile(val char: Char, val fg: Color = Color.White, val bg: Color = Color.Black) extends Tile

class TestMap extends TileMap {
  def apply(x: Int, y: Int): Tile = {
    if (x == 0 && y == 0) new SimpleTile('@') else
    if ((x % 10 == 0) && (y % 5 == 0)) new SimpleTile('X') else new SimpleTile('.')
  }
}

class ShipPlan(modules: Array[Array[Option[ModulePlan]]]) extends TileMap {
  val numModuleX = 7
  val numModuleY = 5
  val moduleWidth = 11
  val moduleHeight = 6

  val bulkheadTile = new SimpleTile('x')
  val emptySpaceTile = new SimpleTile(' ')
  val airlockTile = new SimpleTile('X')
  val doorTile = new SimpleTile('.')
  val exteriorTile = new SimpleTile('O')

  def startingPosition(): Option[(Int, Int)] = {
    getModule(Module.Helm) match {
      case m :: ms => m._1.find('*') match {
        case p :: ps => Some(shipCoords(m._2, p))
        case Nil => None
      }
      case Nil => None
    }
  }

  def getModule(module: Module): List[(ModulePlan, (Int, Int))] = {
      val indices = for{
        (row, i) <- modules.iterator.zipWithIndex
        (col, j) <- row.iterator.zipWithIndex
        m <- col
        if ( (m.module.name == module.name) && (m.primary))
      } yield (m, (i, j))

      indices.toList
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
}

case class SkillType(name: String, color: Color)

object SkillType {
  val Command = SkillType("Command", Color.LightYellow)
  val Science = SkillType("Science", Color.Blue)
  val Engineer = SkillType("Engineer", Color.Green)
  val Weapons = SkillType("Weapons", Color.Red)
  val None = SkillType("None", Color.LightGrey)
}

case class Module(name: String, skillType: SkillType)
object Module {
  import SkillType._

  val Helm = Module("Helm", Command)
  val Cargo = Module("Cargo", None)
  val PowerPlant = Module("PowerPlant", Engineer)
  val HyperDrive = Module("HyperDrive", Science)
  val Phasers = Module("Phasers", Weapons)
  val Sensors = Module("Sensors", Science)
}

class ModulePlan(val module: Module, tiles: Array[Array[Tile]], val primary: Boolean) {
  val width = 10
  val height = 5

  def apply(x: Int, y: Int): Option[Tile] = {
    if ((x < width) && (y < height)) {
      Some(tiles(x)(y))
    } else None
  }

  def find(c: Char): List[(Int, Int)] = {
    val indices = for{
      (row, i) <- tiles.iterator.zipWithIndex
      (col, j) <- row.iterator.zipWithIndex
      if( col.char == c)
    } yield i -> j

    indices.toList
  }
}

object ShipPlan {
  def fromPlan(plan: String): ShipPlan = {
    val modules = ofDim[Option[ModulePlan]](7, 5)
    val lines = plan.lines.zipWithIndex

    for ((line, lineNum) <- lines) {
      var rowNum = 0
      for (c <- line) {
        modules(rowNum)(lineNum) = toModulePlan(c)
        rowNum  += 1
      }
    }

    new ShipPlan(modules)
  }

  def toModulePlan(c: Char): Option[ModulePlan] = {
    c.toUpper match {
      case 'H' => Some(ModulePlan.fromPlan(Module.Helm, Character.isUpperCase(c), Modules.Helm0))
      case 'C' => Some(ModulePlan.fromPlan(Module.Cargo, Character.isUpperCase(c), Modules.Cargo0))
      case 'D' => Some(ModulePlan.fromPlan(Module.HyperDrive, Character.isUpperCase(c), Modules.HyperDrive0))
      case 'P' => Some(ModulePlan.fromPlan(Module.PowerPlant, Character.isUpperCase(c), Modules.PowerPlant0))
      case 'W' => Some(ModulePlan.fromPlan(Module.Phasers, Character.isUpperCase(c), Modules.Phasers0))

      case _ => None
    }
  }

  val ship0 = fromPlan(ShipPlans.shipPlan0)
  val ship1 = fromPlan(ShipPlans.shipPlan1)
  val ship2 = fromPlan(ShipPlans.shipPlan2)
}

object ModulePlan {
  def fromPlan(module: Module, primary: Boolean, plan: String): ModulePlan = {
    val tiles = ofDim[Tile](10, 5)
    val lines = plan.lines.zipWithIndex

    for ((line, lineNum) <- lines) {
      var rowNum = 0
      for (c <- line) {
        tiles(rowNum)(lineNum) = toTile(c, module)
        rowNum  += 1
      }
    }
    new ModulePlan(module, tiles, primary)
  }

  def toTile(c: Char, module: Module): Tile = {
    c match {
      case '.' =>  new SimpleTile(c, Color.LightGrey, Color.Black)
      case _ =>  new SimpleTile(c, module.skillType.color, Color.Black)
    }

  }
}

object Modules {
  val Helm0 =
    """xxxx..xxxx
      |x#.....xxx
      |........*x
      |x#.....xxx
      |xxxx..xxxx""".stripMargin

  val helm1 =
    """xxxx..xxxx
      |x#......xx
      |.......#xx
      |x#......xx
      |xxxx..xxxx""".stripMargin

  val helm2 =
    """xxxx..xxxx
      |xx#.....xx
      |.......#xx
      |xx#.....xx
      |xxxx..xxxx""".stripMargin

  val Cargo0 =
    """xx.......x
      |.........x
      |..........
      |.........x
      |xx......xx""".stripMargin

  val HyperDrive0 =
    """xx.......x
      |.........x
      |..........
      |.........x
      |xx......xx""".stripMargin

  val PowerPlant0 =
    """xx.......x
      |.........x
      |..........
      |.........x
      |xx......xx""".stripMargin

  val Phasers0 =
    """..........
      |..xxxx....
      |...*xxxxxx
      |..xxxx....
      |..........""".stripMargin

  val cargo1 =
    """..........
      |..........
      |..........
      |..........
      |..........""".stripMargin

  val none =
    """..........
      |..........
      |..........
      |..........
      |..........""".stripMargin
}

object ShipPlans {
  val shipPlan0 =
    """.......
      |..c....
      |..DPCH.
      |..c....
      |.......""".stripMargin

  val shipPlan1 =
    """..w....
      |.dc....
      |..cPCH.
      |.dc....
      |..w....""".stripMargin

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