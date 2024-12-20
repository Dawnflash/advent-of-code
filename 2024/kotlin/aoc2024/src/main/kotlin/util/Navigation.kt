package cz.dawnflash.aoc2024.util

import kotlin.math.abs

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
        val cardinals = listOf(N, E, S, W)
        val diagonals = listOf(NE, SE, SW, NW)
        fun fromChar(c: Char): Direction = when (c) {
            '>' -> E
            '<' -> W
            '^' -> N
            'v' -> S
            else -> error("unknown direction")
        }
    }

    fun turnAround() = turnRight().turnRight()
    fun turnLeft() = turnRight().turnRight().turnRight()
}

open class Map2D<T>(val data: List<MutableList<T>>) {
    val h = data.size
    val w = if (h == 0) 0 else data[0].size

    fun at(p: Point): T = data[p.second][p.first]
    fun set(p: Point, v: T) {
        data[p.second][p.first] = v
    }

    fun atSafe(p: Point): T? = if (isInside(p)) at(p) else null
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

    fun print(transform: (T) -> String = { it.toString() }) = data.forEach { row ->
        println(row.joinToString("", transform = transform))
    }

    fun toGraph(isFree: (T) -> Boolean): Graph<Point> {
        val vertices = findAll { isFree(it) }.toHashSet()
        val edges = HashMap(vertices.associateWith { v ->
            neighbors4(v).filter { isFree(at(it)) }.map { it to 1.0 }
        })
        return Graph(vertices, edges)
    }

    companion object {
        fun <T> from(data: List<List<T>>): Map2D<T> {
            return Map2D(data.map { it.toMutableList() })
        }
    }
}

operator fun Point.plus(o: Point) = this.first + o.first to this.second + o.second
operator fun Point.minus(o: Point) = this.first - o.first to this.second - o.second
operator fun Point.times(o: Int) = this.first * o to this.second * o
operator fun Point.rem(o: Int) = this.first % o to this.second % o
operator fun Point.rem(o: Point) = this.first % o.first to this.second % o.second
fun Point.step(dir: Direction): Point = this.first + dir.offset.first to this.second + dir.offset.second
fun <T> List<List<T>>.at(p: Point): T = this[p.second][p.first]
fun Point.distanceFrom(o: Point): Int = abs(o.first - this.first) + abs(o.second - this.second)
fun Point.neighborhood(min: Int, max: Int): List<Point> {
    return (this.first - max..this.first + max).flatMap { x ->
        (this.second - max..this.second + max).map { y ->
            val p = x to y
            val d = p.distanceFrom(this)
            p.takeIf { d in min..max }
        }
    }.filterNotNull()
}