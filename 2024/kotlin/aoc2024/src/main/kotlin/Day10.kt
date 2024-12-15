package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Point
import cz.dawnflash.aoc2024.util.Map2D

class Day10 : Day() {
    override val sampleChecks = "36" to "81"
    override val checks = "587" to "1340"

    private fun numPaths(map: Map2D<Int>, pos: Point, cur: Int = 0): Int = when (cur) {
        9 -> 1
        else -> map.neighbors4(pos).filter { map.at(it) == cur + 1 }.sumOf { numPaths(map, it, cur + 1) }
    }

    private fun reachableEnds(map: Map2D<Int>, pos: Point, cur: Int = 0): Set<Point> = when (cur) {
        9 -> setOf(pos)
        else -> map.neighbors4(pos).filter { map.at(it) == cur + 1 }.map { reachableEnds(map, it, cur + 1) }
            .fold(setOf()) { acc, e -> acc.union(e) }
    }

    override fun solution1(input: List<String>): String {
        val map = Map2D.from(input.map { line -> line.map { it.digitToInt() } })
        return map.findAll { it == 0 }.sumOf { reachableEnds(map, it).size }.toString()
    }

    override fun solution2(input: List<String>): String {
        val map = Map2D.from(input.map { line -> line.map { it.digitToInt() } })
        return map.findAll { it == 0 }.sumOf { numPaths(map, it) }.toString()
    }
}