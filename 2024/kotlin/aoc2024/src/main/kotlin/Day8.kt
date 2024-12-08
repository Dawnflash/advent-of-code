package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.*

class Day8 : Day() {
    override val sampleChecks = "14" to "34"
    override val checks = "413" to ""

    private fun getAntinodes(a: Point, b: Point, dims: Point, part2: Boolean = false): List<Point> {
        fun isBounded(p: Point) = p.first in 0 until dims.first && p.second in 0 until dims.second
        val delta = a - b
        if (!part2) {
            return listOf(a + delta, b - delta).filter { isBounded(it) }
        }
        val ret = mutableListOf<Point>()
        var p = a
        while (isBounded(p)) {
            ret.add(p)
            p -= delta
        }
        p = b
        while (isBounded(p)) {
            ret.add(p)
            p += delta
        }
        return ret
    }

    private fun solution(input: List<String>, part2: Boolean): String {
        val dims = input[0].length to input.size
        val antennas = HashMap<Char, MutableSet<Point>>()
        input.forEachIndexed { y, row ->
            row.forEachIndexed { x, c ->
                if (c != '.') {
                    antennas.getOrPut(c) { mutableSetOf() }.add(x to y)
                }
            }
        }
        val antinodes = mutableSetOf<Point>()
        antennas.entries.forEach { locs ->
            locs.value.forEachIndexed { l, lpos ->
                locs.value.filterIndexed { r, _ -> r > l }
                    .forEach { rpos -> getAntinodes(lpos, rpos, dims, part2).forEach { antinodes.add(it) } }
            }
        }
        return antinodes.size.toString()
    }

    override fun solution1(input: List<String>) = solution(input, false)
    override fun solution2(input: List<String>) = solution(input, true)
}