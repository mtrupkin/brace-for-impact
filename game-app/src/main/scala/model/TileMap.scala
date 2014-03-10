package model

import org.flagship.console.Size
import Array._

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
}

class SimpleTile(val char: Char) extends Tile

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

  def apply(x: Int, y: Int): Tile = {
    if ((x > 0) && (y > 0)) {
      toModule(x-1, y-1)
    } else {
      emptySpaceTile
    }
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
    if ((mx > 0) && (my > 0) &&(mx < numModuleX) && (my < numModuleY)) {
      modules(mx)(my) match {
        case Some(module) => true
        case None => false
      }
    } else {
      false
    }
  }
}

class ModulePlan(tiles: Array[Array[Tile]]) {
  val width = 10
  val height = 5

  def apply(x: Int, y: Int): Option[Tile] = {
    if ((x < width) && (y < height)) {
      Some(tiles(x)(y))
    } else None
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
    c match {
      case 'H' => Some(ModulePlan.fromPlan(Modules.helm0))
      case 'C' => Some(ModulePlan.fromPlan(Modules.cargo0))
      case 'D' => Some(ModulePlan.fromPlan(Modules.cargo0))
      case 'P' => Some(ModulePlan.fromPlan(Modules.cargo0))
      case _ => None
    }
  }

  val ship0 = fromPlan(ShipPlans.shipPlan0)
  val ship1 = fromPlan(ShipPlans.shipPlan1)

}

object ShipPlans {
  val shipPlan0 =
    """.......
      |..C....
      |..DPCH.
      |..C....
      |.......""".stripMargin

  val shipPlan1 =
    """.......
      |..C....
      |..DPCH.
      |..C....
      |.......""".stripMargin
}

object ModulePlan {
  def fromPlan(plan: String): ModulePlan = {
    val tiles = ofDim[Tile](10, 5)
    val lines = plan.lines.zipWithIndex

    for ((line, lineNum) <- lines) {
      var rowNum = 0
      for (c <- line) {
        tiles(rowNum)(lineNum) = toTile(c)
        rowNum  += 1
      }
    }
    new ModulePlan(tiles)
  }

  def toTile(c: Char): Tile = {
    new SimpleTile(c)
  }
}


class CargoModule extends TileMap {
  val map =
   """|XXXXXXXXXX
      |X........X
      |X.........
      |X.........
      |XXXXXX....
      |X.........
      |X.........
      |X.........
      |X.........
      |X.........
    """.stripMargin
  def apply(x: Int, y: Int): Tile = {
    val c: Char = map.charAt(y*12 + x)
    new SimpleTile(c)
  }
}

object Modules {
  val helm0 =
    """xxxx..xxxx
      |x#.....xxx
      |........#x
      |x#.....xxx
      |xxxx..xxxx""".stripMargin

  val helm1 =
    """
      |xxxx  xxxx
      |x#      xx
      |       #xx
      |x#      xx
      |xxxx  xxxx
    """.stripMargin

  val helm2 =
    """
      |xxxx  xxxx
      |xx#     xx
      |       #xx
      |xx#     xx
      |xxxx  xxxx
    """.stripMargin

  val cargo0 =
    """x........x
      |.........x
      |..........
      |.........x
      |xx......xx""".stripMargin

  val cargo1 =
    """
      |..........
      |..........
      |..........
      |..........
      |..........
    """.stripMargin

  val bulkHeadHorizontal =
    """XXXX..XXXX"""

  val bulkHeadVertical =
    """X
      |X
      |.
      |X
      |X""".stripMargin

  val none =
    """..........
      |..........
      |..........
      |..........
      |..........""".stripMargin
}

object Ships {
}