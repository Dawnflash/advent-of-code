package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.transpose

class Day25 : Day() {
    override val sampleChecks = "3" to ""
    override val checks = "3107" to ""

    private class Problem(val keys: List<List<Int>>, val locks: List<List<Int>>) {
        fun overlaps(): Int = keys.sumOf { key ->
            locks.count { lock -> key.zip(lock).all { (k, l) -> k + l <= SIZE } }
        }

        companion object {
            const val SIZE = 5
            fun from(input: List<String>): Problem {
                val keys = mutableListOf<List<Int>>()
                val locks = mutableListOf<List<Int>>()

                for (obj in input.joinToString("\n").split("\n\n")) {
                    val conv = obj.lines().subList(1, SIZE + 1).transpose().map { line -> line.count { it == '#' } }
                    if (obj[0] == '#') locks.add(conv) else keys.add(conv)
                }

                return Problem(keys, locks)
            }
        }
    }

    override fun solution1(input: List<String>) = Problem.from(input).overlaps().toString()
// part 2 is free
}