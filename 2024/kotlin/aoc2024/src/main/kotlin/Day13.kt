package cz.dawnflash.aoc2024

import kotlin.math.min

class Day13 : Day() {
    override val sampleChecks = "480" to "875318608908"
    override val checks = "28138" to "108394825772874"

    private data class Game(val a: Pair<Long, Long>, val b: Pair<Long, Long>, val p: Pair<Long, Long>) {
        fun solve(): Long? {
            val bc = (a.second * p.first - a.first * p.second) / (a.second * b.first - a.first * b.second)
            val ac = (p.first - bc * b.first) / a.first

            if (p.first == a.first * ac + b.first * bc && p.second == a.second * ac + b.second * bc) {
                return bc + 3 * ac
            }

            return null
        }
    }

    private fun parse(input: List<String>, part2: Boolean): List<Game> {
        val rgx = Regex("""(\d+)\D+(\d+)""")
        return input.filter { it.isNotEmpty() }
            .map { rgx.find(it)!!.groupValues.let { gs -> gs[1].toLong() to gs[2].toLong() } }
            .windowed(3, 3).map {
                when (part2) {
                    false -> Game(it[0], it[1], it[2])
                    true -> Game(it[0], it[1], it[2].first + 10000000000000L to it[2].second + 10000000000000L)
                }
            }
    }

    override fun solution(input: List<String>, part2: Boolean): String {
        return parse(input, part2).sumOf { it.solve() ?: 0L }.toString()
    }
}