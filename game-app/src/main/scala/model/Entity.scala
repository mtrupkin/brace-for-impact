package model

import org.flagship.console.Point
import flagship.console.terminal.Color

/**
 * User: mtrupkin
 * Date: 12/18/13
 */
trait Entity {
  def name: String
  def position: Point
  def char: Char
  def color: Color
  def stamina: Int
  def movement: Int
  def attack(value: Int)
  def move(p: Point): Point
  def alive: Boolean
  def setPosition(p: Point)
  def movePosition(p: Point): Point
  def resetMovement()
}

class BaseEntity(
  val name: String,
  var position: Point = Point.Origin,
  var char: Char = '@',
  var color: Color = Color.White,
  var stamina: Int = 2,
  val totalMovement: Int = 2) extends Entity {

  var movement: Int = totalMovement

  def attack(value: Int) {
    stamina = stamina - value
    if (!alive) {
      char = '.'
    }
  }

  def alive: Boolean = (stamina > 0)

  def move(p: Point): Point = { position = position.move(p); movement -= 1; position }
  def movePosition(p: Point): Point = { position = p.copy(); movement -= 1; position }
  def setPosition(p: Point) { position = p.copy() }

  def resetMovement() {
    movement = totalMovement
  }
}

object Entity {
  def None: Entity = new BaseEntity("None")
}

class Player() extends BaseEntity("Player", stamina = 6) {
  def up() = {position = position.move(Point.Up)}
  def down() = {position = position.move(Point.Down)}
  def left() = {position = position.move(Point.Left)}
  def right() = {position = position.move(Point.Right)}
}
