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
  def entities(): List[Entity]
}

trait Tile {
  def move: Boolean
  def char: Char
  def fg: Color
  def bg: Color
  def activate(shipPlan: ShipPlan, entity: Entity): Unit
}

class SimpleTile(
  val char: Char,
  val move: Boolean = false,
  val fg: Color = Color.White,
  val bg: Color = Color.Black,
  val onActivate: ((ShipPlan, Entity) => Unit) = (s: ShipPlan, e: Entity) => {} ) extends Tile {
  def activate(shipPlan: ShipPlan, entity: Entity): Unit = onActivate(shipPlan, entity)
}

