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
  val widthInTiles: Int
  val heightInTiles: Int

  def apply(x: Int, y: Int): Tile
  def entities(): List[Entity]
  def entity(x: Int, y: Int): Option[Entity]
}

trait Tile {
  def char: Char
  def move: Boolean
  def activation: Option[ModuleType]
  def fg: Color
  def bg: Color
  def activate(game: GameSequence, m:ModuleType, entity: Entity): Unit
}

class SimpleTile(
  val char: Char,
  val move: Boolean = false,
  val activation:  Option[ModuleType] = None,
  val fg: Color = Color.White,
  val bg: Color = Color.Black,
  val onActivate: ((GameSequence, ModuleType, Entity) => Unit) = (game: GameSequence, m:ModuleType, e: Entity) => {} ) extends Tile {
  def activate(game: GameSequence, m:ModuleType, entity: Entity): Unit = onActivate(game, m, entity)
}

