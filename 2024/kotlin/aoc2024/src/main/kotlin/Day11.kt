package cz.dawnflash.aoc2024

import kotlin.math.log10
import kotlin.math.pow

class Day11 : Day() {
    override val sampleChecks = "55312" to ""
    override val checks = "194482" to "232454623677743"

    private fun blinkStone(stone: ULong): List<ULong> {
        val nDigits = log10(stone.toDouble()).toInt() + 1
        val decSplit = 10.0.pow(nDigits / 2).toUInt()
        return when {
            stone == 0UL -> listOf(1UL)
            nDigits % 2 == 0 -> listOf(stone / decSplit, stone % decSplit)
            else -> listOf(stone * 2024UL)
        }
    }

    private fun blink(stones: Map<ULong, ULong>): Map<ULong, ULong> {
        val out = HashMap<ULong, ULong>()
        for ((k, v) in stones) {
            blinkStone(k).forEach { ns ->
                out[ns] = out.getOrDefault(ns, 0UL) + v
            }
        }
        return out
    }

    private fun solution(input: List<String>, blinks: Int): String {
        val stones = input[0].split(" ").map(String::toULong).groupingBy { it }
            .fold(0UL) { acc, _ -> acc + 1UL }

        return (1..blinks).fold(stones) { acc, _ -> blink(acc).also {println(acc.size)} }.values.sum()
            .toString()
    }

    override fun solution1(input: List<String>): String = solution(input, 25)
    override fun solution2(input: List<String>): String = solution(input, 75)
}