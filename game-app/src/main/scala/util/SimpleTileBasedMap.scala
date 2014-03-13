package util

import model.TileMap
import org.newdawn.slick.util.pathfinding.{PathFindingContext, TileBasedMap}

/**
 * Created by mtrupkin on 3/12/14.
 */
class TileBasedMapWrapper(val map: TileMap) extends TileBasedMap {

    def blocked(ctx: PathFindingContext, x: Int, y: Int): Boolean = {
      !map.apply(x, y).move
    }

    def getCost(ctx: PathFindingContext, x: Int, y: Int): Float = 1.0f


    def getHeightInTiles(): Int = {
      map.heightInTiles
    }


    def getWidthInTiles(): Int = {
      map.widthInTiles
    }

    def pathFinderVisited(x: Int, y: Int) {}
}
