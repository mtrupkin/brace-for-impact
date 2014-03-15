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
  val Science = SkillType("Science", Color.LightBlue)
  val Engineer = SkillType("Engineer", Color.Green)
  val Weapons = SkillType("Weapons", Color.Red)
  val None = SkillType("None", Color.LightGrey)
}

case class ModuleStatus(name: String)

object ModuleStatus {
  val Ready = ModuleStatus("Ready")
  val Activated = ModuleStatus("Activated")
  val Damaged = ModuleStatus("Damaged")
}


case class ModuleType(
  name: String,
  skillType: SkillType,
  activate: ((GameSequence, ModuleType, Entity) => Unit) =
    (g: GameSequence, m:ModuleType, e: Entity) => ModuleType.activateModule(g, m)) {
  var status: ModuleStatus = ModuleStatus.Ready
}


object ModuleType {
  import SkillType._

  //def Helm(ship: ShipPlan) = Module("Helm", Command, helmActivate)
  val Helm = ModuleType("Helm", Command, helmActivate)
  val Cargo = ModuleType("Cargo", None, cargoActivate)
  val PowerPlant = ModuleType("Power Plant", Engineer, powerPlantActivate)
  val HyperDrive = ModuleType("Hyper Drive", Engineer)
  val Phasers = ModuleType("Phasers", Weapons, phaserActivate)
  val Sensors = ModuleType("Sensors", Science)
  val Shields = ModuleType("Shields", Science, shieldActivate)
  val Transporter = ModuleType("Transporter", Science)



  def activateModule(game: GameSequence, module: ModuleType): Boolean = {
    if (module.status == ModuleStatus.Activated) {
      game.addMessage(s"${module.name} already activated.")
      false
    } else if (game.ship.power > 0) {
      game.ship.power -= 1
      game.addMessage(s"Activated ${module.name}.")
      module.status = ModuleStatus.Activated
      true
    } else {
      game.addMessage(s"${module.name} requires power to activate.")
      false
    }
  }

  def cargoActivate(game: GameSequence, m:ModuleType, e: Entity) = {
    if (activateModule(game, m)) {
      e.repairArmor(1)
    }
  }

  def helmActivate(game: GameSequence, m:ModuleType, e: Entity) = {
    if (game.shipStatus == ShipStatus.Green) {
      game.addMessage("Hyper space jump activated.")
      game.beginEncounter()
      Encounter.createEncounter(game)
    } else {
      game.addMessage("Helm disabled during RED ALERT.")
    }
  }

  def phaserActivate(game: GameSequence, m: ModuleType, e: Entity) = {
    if (activateModule(game, m)) {
      game.ship.weapons += 1
    }
  }

  def powerPlantActivate(game: GameSequence, module: ModuleType, e: Entity) = {
    if (module.status == ModuleStatus.Activated) {
      game.addMessage(s"${module.name} already activated.")
    } else  {
      game.ship.power += 1
      game.addMessage(s"Activated ${module.name}.")
      module.status = ModuleStatus.Activated
    }
  }

  def shieldActivate(game: GameSequence, m: ModuleType, e: Entity) = {
    if (activateModule(game, m)) {
      game.ship.increaseShields(1)
    }
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
      case '.' =>  new SimpleTile(' ', true, None, Color.LightGrey, Color.Black)
      case '#' =>  new SimpleTile('+', false, Some(module), module.skillType.color, Color.Black, module.activate)
      case _ =>  new SimpleTile('#', false, None, module.skillType.color, Color.Black)
    }
  }
}

object Modules {
  val Helm0 =
    """xxxx..xxxx
      |x#.....xxx
      |........#x
      |x#.....xxx
      |xxxx..xxxx""".stripMargin

  val Cargo0 =
    """xx......xx
      |x..#..#..x
      |..........
      |x..#..#..x
      |xx......xx""".stripMargin

  val Cargo0a =
    """xx......xx
      |x...##...x
      |..........
      |x...##...x
      |xx......xx""".stripMargin

  val HyperDrive0 =
    """xx........
      |xx#.xxxx..
      |xxxxxxx#..
      |xx#.xxxx..
      |xx........""".stripMargin

  val PowerPlant0 =
    """xxxx..xxxx
      |xxx#..#xxx
      |..........
      |xxx#..#xxx
      |xxxx..xxxx""".stripMargin

  val Phasers0 =
    """..........
      |..xxxx....
      |...#xxxx..
      |..xxxx....
      |..........""".stripMargin

  val Phasers0a =
    """..........
      |..xxxx....
      |...#xxxxxx
      |..xxxx....
      |..........""".stripMargin

  val Transporter0 =
    """..........
      |..xx.xxx..
      |.....#xx..
      |..xx.xxx..
      |..........""".stripMargin

  val Shields0 =
    """......xxxx
      |..xx...#xx
      |..........
      |..xx...#xx
      |......xxxx""".stripMargin

  val Shields0a =
    """......xxxx
      |..xx...#xx
      |..xxxxxxxx
      |..xx...#xx
      |......xxxx""".stripMargin

  val Shields0b =
    """......xxxx
      |..xx...#xx
      |..xx......
      |..xx...#xx
      |......xxxx""".stripMargin

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
