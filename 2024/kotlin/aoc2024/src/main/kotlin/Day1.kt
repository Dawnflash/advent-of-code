package cz.dawnflash.aoc2024

import kotlin.math.abs

class Day1 : Day() {
    override val sampleChecks = "11" to "31"
    override val checks = "1651298" to "21306195"

    override fun solution1(input: List<String>) = input
        .map { line -> line.split("   ").let { it[0].toInt() to it[1].toInt() } }
        .unzip()
        .let { (l, r) -> l.sorted().zip(r.sorted()).sumOf { (l, r) -> abs(l - r) } }
        .toString()

    override fun solution2(input: List<String>) = input
        .map { line -> line.split("   ").let { it[0].toInt() to it[1].toInt() } }
        .unzip()
        .let { (l, r) -> l.sumOf { le -> le * r.count { it == le } } }
        .toString()
}