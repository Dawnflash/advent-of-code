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

    fun turnAround() = turnRight().turnRight()
    fun turnLeft() = turnRight().turnRight().turnRight()
}

operator fun Point.plus(o: Point) = this.first + o.first to this.second + o.second
operator fun Point.minus(o: Point) = this.first - o.first to this.second - o.second
fun Point.step(dir: Direction): Point = this.first + dir.offset.first to this.second + dir.offset.second
fun <T> List<List<T>>.at(p: Point): T = this[p.second][p.first]
