package model

import flagship.console.terminal.Color
import scala.Array._
import scala.Some
import org.flagship.console.Point

/**
 * Created by mtrupkin on 3/11/14.
 */
case class SkillType(name: String, color: Color)

object SkillType {
  val Command = SkillType("Command", Color.LightYellow)
  val Science = SkillType("Science", Color.Blue)
  val Engineer = SkillType("Engineer", Color.Green)
  val Weapons = SkillType("Weapons", Color.Red)
  val None = SkillType("None", Color.LightGrey)
}

case class ModuleType(name: String, skillType: SkillType, activate: ((ShipPlan, Entity) => Unit) = (s: ShipPlan, e: Entity) => {})

object ModuleType {
  import SkillType._

  //def Helm(ship: ShipPlan) = Module("Helm", Command, helmActivate)
  val Helm = ModuleType("Helm", Command, helmActivate)
  val Cargo = ModuleType("Cargo", None)
  val PowerPlant = ModuleType("PowerPlant", Engineer)
  val HyperDrive = ModuleType("HyperDrive", Science)
  val Phasers = ModuleType("Phasers", Weapons, phaserActivate)
  val Sensors = ModuleType("Sensors", Science)

  def helmActivate(s: ShipPlan, e: Entity) = {
    println("helm activate")
    Encounter.createEncounter(s)
  }

  def phaserActivate(s: ShipPlan, e: Entity) = {
    println("phaser activate")
    s.entities = Nil
  }
}

class ModulePlan(val module: ModuleType, val tiles: Array[Array[Tile]], val primary: Boolean) {
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

  def getCells(): List[(Tile, (Int, Int))] = {
    val cells = for{
      i <- 0 until width
      j <- 0 until height
      tile = tiles(i)(j)
    } yield (tile, (i, j))

    cells.toList
  }

}



object ModulePlan {
  def fromPlan(module: ModuleType, primary: Boolean, plan: String): ModulePlan = {
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

  def toTile(c: Char, module: ModuleType): Tile = {
    c match {
      case '.' =>  new SimpleTile(c, true, Color.LightGrey, Color.Black)
      case '*' | '#' =>  new SimpleTile(c, true, module.skillType.color, Color.Black, module.activate)
      case _ =>  new SimpleTile(c, false, module.skillType.color, Color.Black)
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
