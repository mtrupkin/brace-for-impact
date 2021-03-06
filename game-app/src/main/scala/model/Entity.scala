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
  def armor: Int
  def repairArmor(amount: Int)
  def movement: Int
  def attack(other: Entity)
  def damage(value: Int)
  def move(p: Point): Point
  def alive: Boolean
  def setPosition(p: Point)
  def movePosition(p: Point): Point
  def resetMovement()
  def endMovement()
}

class BaseEntity(
  val name: String,
  var position: Point = Point.Origin,
  var char: Char = '@',
  var color: Color = Color.White,
  var stamina: Int = 2,
  var armor: Int = 2,
  var movement: Int = 2,
  var bitcoins: Int = 0) extends Entity {

  val maxMovement = movement
  val maxStamina = stamina
  val maxArmor = armor

//  var movement: Int = maxMovement
//  var armor: Int = maxArmor
//  var stamina: Int = maxStamina

  def attack(other: Entity) {
    other.damage(1)
    endMovement
  }

  def damage(amount: Int) {
    val remaining = armor - amount
    if (remaining < 0) {
      armor = 0
      stamina += remaining
    } else {
      armor = remaining
    }
    if (!alive) {
      char = '.'
      color = Color.Red
    }
  }

  def repairArmor(amount: Int) {
    armor += 1
    if (armor > maxArmor) {armor = maxArmor}
  }

  def activate(game:GameSequence, module:ModuleType, p: Point) {
    endMovement
    game.ship(p.x, p.y).activate(game, module, this)
  }

  def alive: Boolean = (stamina > 0)

  def move(p: Point): Point = { position = position.move(p); movement -= 1; position }
  def movePosition(p: Point): Point = { position = p.copy(); movement -= 1; position }
  def setPosition(p: Point) { position = p.copy() }

  def resetMovement() = movement = maxMovement
  def endMovement() { movement = -1 }
}

object Entity {
  def None: Entity = new BaseEntity("None")
}

class Player() extends BaseEntity("Player", stamina = 7, armor = 7, movement = 5, color = Color.Yellow) {
  def up() = {position = position.move(Point.Up)}
  def down() = {position = position.move(Point.Down)}
  def left() = {position = position.move(Point.Left)}
  def right() = {position = position.move(Point.Right)}
}
