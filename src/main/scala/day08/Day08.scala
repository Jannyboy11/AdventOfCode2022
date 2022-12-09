package day08

import scala.io.Source;
import Direction.*

val source = Source.fromResource("day08.in")
val grid: Grid = source.getLines().map(_.map(_ - '0')).toIndexedSeq

type Height = Int
type Grid = IndexedSeq[IndexedSeq[Height]]
type Coordinate = (Int, Int)

extension (grid: Grid)
    def width = grid(0).size
    def height = grid.size

def go1(grid: Grid): Set[Coordinate] =
    var set = Set[Coordinate]()
    var y = 0

    while y < grid.height do
        var maxLeft: Height = -1
        var maxRight: Height = -1
        var x = 0
        while x < grid.width do
            val value = grid(y)(x)
            if value > maxLeft then
                set += (x, y)
                maxLeft = value
            x += 1
        while x > 0 do
            x -= 1
            val value = grid(y)(x)
            if value > maxRight then
                set += (x, y)
                maxRight = value
        y += 1

    var x = 0
    while x < grid.width do
        var maxTop: Height = -1
        var maxBottom: Height = -1
        var y = 0
        while y < grid.width do
            val value = grid(y)(x)
            if value > maxTop then
                set += (x, y)
                maxTop = value
            y += 1
        while y > 0 do
            y -= 1
            val value = grid(y)(x)
            if value > maxBottom then
                set += (x, y)
                maxBottom = value
        x += 1

    set
end go1

enum Direction:
    case Left, Right, Up, Down
type Distance = Int
case class CacheKey(x: Int, y: Int, height: Height, direction: Direction)
type Cache = Map[CacheKey, Distance]

def go2(grid: Grid, cacheKey: CacheKey, cache: Cache): Cache =
    if cache.contains(cacheKey) then cache
    else
        val rightX = grid.width - 1
        val bottomY = grid.height - 1
        cacheKey match
            case CacheKey(0, _, _, Left) =>
                (0 to 9).foldLeft(cache){ case (c, h) => c.updated(cacheKey.copy(height = h), 0) }
            case CacheKey(x, _, _, Right) if x == rightX =>
                (0 to 9).foldLeft(cache){ case (c, h) => c.updated(cacheKey.copy(height = h), 0) }
            case CacheKey(_, 0, _, Up) =>
                (0 to 9).foldLeft(cache){ case (c, h) => c.updated(cacheKey.copy(height = h), 0) }
            case CacheKey(_, y, _, Down) if y == bottomY =>
                (0 to 9).foldLeft(cache){ case (c, h) => c.updated(cacheKey.copy(height = h), 0) }
            case CacheKey(x, y, _, direction) =>
                val (otherX, otherY) = direction match
                    case Left => (x-1, y); case Right => (x+1, y); case Up => (x, y-1); case Down => (x, y+1)
                val otherHeight = grid(otherY)(otherX)
                val neighbourKey = cacheKey.copy(x = otherX, y = otherY)
                val neighbourCache = cache.get(neighbourKey) match
                    case Some(_) => cache
                    case None => go2(grid, neighbourKey, cache)
                (0 to 9).foldLeft(neighbourCache) { case (c, h) =>
                    val neighbourDistance = if h > otherHeight then neighbourCache(neighbourKey.copy(height = h)) else 0
                    c.updated(cacheKey.copy(height = h), neighbourDistance + 1)
                }

def scenicScore(coordinate: Coordinate, grid: Grid, cache: Cache): Int = coordinate match
    case (x, y) => Direction.values.map(d => cache(CacheKey(x, y, grid(y)(x), d))).product

@main def main: Unit = {

    val result1 = go1(grid).size
    println(result1)

    val result2 = {
        val cacheKeys = for y <- 0 until grid.height; x <- 0 until grid.width; d <- Direction.values yield CacheKey(x, y, grid(y)(x), d)
        val cache = cacheKeys.foldLeft[Cache](Map.empty) { case (c, k) => go2(grid, k, c) }
        val scores = for y <- 0 until grid.height; x <- 0 until grid.width yield scenicScore((x, y), grid, cache)
        scores.max
    }
    println(result2)

}