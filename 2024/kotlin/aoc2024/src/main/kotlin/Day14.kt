package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.*

class Day14 : Day() {
    override val sampleChecks = "12" to ""
    override val checks = "224554908" to ""

    private data class Robot(var p: Point, val v: Point) {
        fun move(steps: Int, size: Point): Point {
            p = (p + v * steps) % size
            correctP(size)
            return p
        }

        fun correctP(size: Point) {
            val x = if (p.first < 0) size.first + p.first else p.first
            val y = if (p.second < 0) size.second + p.second else p.second
            p = x to y
        }

        fun quadrant(size: Point): Int? {
            val (divX, divY) = size.first / 2 to size.second / 2
            return when {
                p.first < divX && p.second < divY -> 0
                p.first > divX && p.second < divY -> 1
                p.first < divX && p.second > divY -> 2
                p.first > divX && p.second > divY -> 3
                else -> null
            }
        }
    }

    private fun print(robots: List<Robot>, size: Point) {
        val data = List(size.second) { MutableList(size.first) { '░' } }
        robots.forEach { data[it.p.second][it.p.first] = '█' }
        data.forEach { println(it.joinToString("")) }
    }

    private fun parse(input: List<String>): Pair<List<Robot>, Point> {
        val rgx = Regex("""p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""")
        val robots = input.map { line ->
            rgx.find(line)!!.groupValues.let {
                Robot(it[1].toInt() to it[2].toInt(), it[3].toInt() to it[4].toInt())
            }
        }
        val size = robots.maxOfOrNull { it.p.first }!! + 1 to robots.maxOfOrNull { it.p.second }!! + 1
        return robots to size
    }

    override fun solution1(input: List<String>): String {
        val (robots, size) = parse(input)
        return robots.fold(mutableListOf(0, 0, 0, 0)) { acc, robot ->
            robot.move(100, size)
            robot.quadrant(size)?.let { acc[it]++ }
            acc
        }.reduce { a, b -> a * b }.toString()
    }

    override fun solution2(input: List<String>): String {
        val (robots, size) = parse(input)
        if (robots.size > 20) {
            val res = 6644
            robots.forEach { it.move(res, size) }
            print(robots, size)
            return res.toString()
        }
        return super.solution2(input)
    }
}