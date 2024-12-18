package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Map2D
import cz.dawnflash.aoc2024.util.Point
import cz.dawnflash.aoc2024.util.distanceFrom
import kotlin.math.max

class Day18 : Day() {
    override val sampleChecks = "22" to "6,1"
    override val checks = "270" to ""
    override val notChecks = arrayOf("") to arrayOf("2871")

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

    override fun solution1(input: List<String>): String {
        val (map, bytes) = parse(input)
        val steps = if (map.w <= 7) 12 else 1024
        return minPathAfter(map, bytes, steps).toString()
    }

    override fun solution2(input: List<String>): String {
        val (map, bytes) = parse(input)
        val steps = if (map.w <= 7) 12 else 1024
        val cutoff = (steps..bytes.size).find { minPathAfter(map, bytes, it) == null }!!
        return bytes[cutoff - 1].let { "${it.first},${it.second}" }
    }
}