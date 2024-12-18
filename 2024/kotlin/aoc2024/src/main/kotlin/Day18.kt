package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Map2D
import cz.dawnflash.aoc2024.util.Point
import cz.dawnflash.aoc2024.util.distanceFrom
import kotlin.math.max

class Day18 : Day() {
    override val sampleChecks = "22" to "6,1"
    override val checks = "270" to "51,40"

    private fun parse(input: List<String>): Pair<Map2D<Boolean>, List<Point>> {
        val pts = input.map { line -> line.split(",").let { it[0].toInt() to it[1].toInt() } }
        val size = pts.maxOfOrNull { max(it.first, it.second) }!! + 1
        return Map2D(List(size) { MutableList(size) { false } }) to pts
    }

    private fun minPathAfter(map: Map2D<Boolean>, bytes: List<Point>, steps: Int): Int? {
        map.findAll { it }.forEach { map.set(it, false) }
        bytes.subList(0, steps).forEach { map.set(it, true) }
        val start = 0 to 0
        val end = map.w - 1 to map.h - 1
        return map.toGraph { !it }.shortestPathLength(start, end) { it.distanceFrom(end).toDouble() }?.toInt()
    }

    // finds the first value in a closed interval satisfying a predicate
    private fun binSearch(bounds: Point, predicate: (Int) -> Boolean): Int {
        var (low, high) = bounds
        while (low < high) {
            val mid = (low + high) / 2
            if (predicate(mid)) {
                high = mid
            } else {
                low = mid + 1
            }
        }
        return low
    }

    override fun solution1(input: List<String>): String {
        val (map, bytes) = parse(input)
        val steps = if (map.w <= 7) 12 else 1024
        return minPathAfter(map, bytes, steps).toString()
    }

    override fun solution2(input: List<String>): String {
        val (map, bytes) = parse(input)
        val steps = if (map.w <= 7) 12 else 1024
        val cutoff = binSearch(steps to bytes.size) {
            minPathAfter(map, bytes, it) == null
        }
        return bytes[cutoff - 1].let { "${it.first},${it.second}" }
    }
}