package cz.dawnflash.aoc2024

class Day2 : Day() {
    override val sampleChecks = "2" to "4"
    override val checks = "680" to "710"

    override fun solution1(input: List<String>) = input
        .map { line -> line.split(' ').map { it.toInt() } }
        .count { isSafe(it) }
        .toString()

    override fun solution2(input: List<String>) = input
        .map { line -> line.split(' ').map { it.toInt() } }
        .count { levels -> isSafe(levels) || levels.indices.any { isSafe(levels.filterIndexed { n, _ -> n != it }) } }
        .toString()

    private fun isSafe(levels: List<Int>) = levels.zipWithNext { a, b -> a > b && a - b < 4 }.all { it }
            || levels.zipWithNext { a, b -> a < b && b - a < 4 }.all { it }
}