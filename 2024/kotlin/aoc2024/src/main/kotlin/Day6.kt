package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.*

class Day6 : Day() {
    override val sampleChecks = "41" to "6"
    override val checks = "5564" to "1976"

    private fun readMap(input: List<String>): Pair<Point, Map2D<Boolean>> {
        var pos: Point = 0 to 0
        val map =
            input.mapIndexed { y, line ->
                line.mapIndexed { x, ch ->
                    when (ch) {
                        '#' -> true
                        '^' -> false.also { pos = x to y }
                        else -> false
                    }
                }
            }
        return pos to Map2D.from(map)
    }

    // returns true if out of bounds, false if looped + visited points
    private fun simulate(
        map: Map2D<Boolean>,
        start: Point,
        extraObstacle: Point? = null
    ): Pair<Boolean, Set<Point>> {
        var direction = Direction.N
        val visited = mutableSetOf(start to direction)
        var pos = start

        while (true) {
            val newPos = pos.step(direction)
            if (!map.isInside(newPos)) break
            when {
                map.at(newPos) || (extraObstacle != null && extraObstacle == newPos) ->
                    direction = direction.turnRight()

                visited.contains(newPos to direction) ->
                    return false to visited.map { it.first }.toSet()

                else -> {
                    visited.add(newPos to direction)
                    pos = newPos
                }
            }
        }

        return true to visited.map { it.first }.toSet()
    }

    override fun solution1(input: List<String>): String {
        val (pos, map) = readMap(input)
        return simulate(map, pos).second.size.toString()
    }

    override fun solution2(input: List<String>): String {
        val (pos, map) = readMap(input)
        val candidates = simulate(map, pos).second.filter { it != pos }
        return candidates.count { !simulate(map, pos, it).first }.toString()
    }
}
