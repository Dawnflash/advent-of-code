package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.*

class Day20 : Day() {
    override val sampleChecks = "0" to "0"
    override val checks = "1463" to "985332"

    data class RaceTrack(val map: Map2D<Boolean>, val start: Point, val end: Point) {
        fun cheats(range: Int): Map<Int, Int> {
            val graph = map.toGraph { !it }
            val (distances, from) = graph.distancesFrom(end)!!
            val path = Graph.buildPath(from, end, start)
            val cheats = HashMap<Int, Int>()
            for ((i, p) in path.withIndex()) {
                val remDist = path.size - 1 - i
                // cheat = jump to a point in [2, range] distance
                p.neighborhood(2, range).filter { map.atSafe(it) == false }.forEach { newStart ->
                    val newLen = distances[newStart]?.toInt()
                    if (newLen != null) {
                        val diff = remDist - (newLen + newStart.distanceFrom(p)) // add the cheat step size
                        if (diff > 0) {
                            cheats[diff] = cheats.getOrDefault(diff, 0) + 1
                        }
                    }
                }
            }
            return cheats
        }
    }

    private fun parse(input: List<String>): RaceTrack {
        var start = 0 to 0
        var end = start
        val map = Map2D.from(input.mapIndexed { y, line ->
            line.mapIndexed { x, it ->
                when (it) {
                    '#' -> true
                    'S' -> false.also { start = x to y }
                    'E' -> false.also { end = x to y }
                    else -> false
                }
            }
        })
        return RaceTrack(map, start, end)
    }

    override fun solution1(input: List<String>): String {
        val track = parse(input)
        return track.cheats(2).filter { it.key >= 100 }.values.sum().toString()
    }

    override fun solution2(input: List<String>): String {
        val track = parse(input)
        return track.cheats(20).filter { it.key >= 100 }.values.sum().toString()
    }
}