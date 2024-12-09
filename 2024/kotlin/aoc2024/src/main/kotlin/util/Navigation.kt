package cz.dawnflash.aoc2024.util

typealias Point = Pair<Int, Int>

enum class Direction(val offset: Point) {
    N(0 to -1),
    NE(1 to -1),
    E(1 to 0),
    SE(1 to 1),
    S(0 to 1),
    SW(-1 to 1),
    W(-1 to 0),
    NW(-1 to -1);

    fun turnRight() = when (this) {
        N -> E
        NE -> SE
        E -> S
        SE -> SW
        S -> W
        SW -> NW
        W -> N
        NW -> NE
    }

    companion object {
        val cardinals = listOf(Direction.N, Direction.E, Direction.S, Direction.W)
        val diagonals = listOf(Direction.NE, Direction.SE, Direction.SW, Direction.NW)
    }

    fun turnAround() = turnRight().turnRight()
    fun turnLeft() = turnRight().turnRight().turnRight()
}

data class Map2D<T>(val data: List<List<T>>) {
    val h = data.size
    val w = if (h == 0) 0 else data[0].size

    fun at(p: Point): T = data[p.second][p.first]
    fun atSafe(p: Point): T? = at(p).takeIf { isInside(p) }
    fun isInside(p: Point) = p.first in 0..<w && p.second in 0..<h
    fun dims(): Point = w to h
    fun isValid(): Boolean = data.all { it.size == w }
    fun findAll(predicate: (T) -> Boolean): List<Point> = data.flatMapIndexed { y, row ->
        row.mapIndexedNotNull { x, i -> (x to y).takeIf { predicate(i) } }
    }

    fun neighbors(p: Point, dirs: List<Direction>): List<Point> = dirs.mapNotNull { dir ->
        p.step(dir).takeIf { isInside(it) }
    }
    fun neighbors4(p: Point) = neighbors(p, Direction.cardinals)
    fun neighbors8(p: Point) = neighbors(p, Direction.entries)
}

operator fun Point.plus(o: Point) = this.first + o.first to this.second + o.second
operator fun Point.minus(o: Point) = this.first - o.first to this.second - o.second
fun Point.step(dir: Direction): Point = this.first + dir.offset.first to this.second + dir.offset.second
fun <T> List<List<T>>.at(p: Point): T = this[p.second][p.first]
