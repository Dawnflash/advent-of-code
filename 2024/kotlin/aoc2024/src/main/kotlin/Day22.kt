package cz.dawnflash.aoc2024

class Day22 : Day() {
    override val sampleChecks = "37990510" to "23"
    override val checks = "13429191512" to "1582"

    private fun gen(state: Long): Long {
        return listOf<(Long) -> Long>(
            { it.shl(6) },
            { it.shr(5) },
            { it.shl(11) }
        ).fold(state) { acc, f ->
            acc.xor(f(acc)) % 16777216L
        }
    }

    override fun solution1(input: List<String>): String {
        return input.sumOf { (1..2000).fold(it.toLong()) { acc, _ -> gen(acc) } }.toString()
    }

    override fun solution2(input: List<String>): String {
        val seqCounter = hashMapOf<List<Long>, Long>()
        val visited = hashSetOf<List<Long>>()
        for (line in input) {
            val bananas = (1..2000).runningFold(line.toLong()) { acc, _ -> gen(acc) }.map { it % 10 }
            val diffs = bananas.windowed(2).map { it[1] - it[0] }
            visited.clear()
            for ((i, seq) in diffs.windowed(4).withIndex()) {
                if (seq !in visited) {
                    visited.add(seq)
                    seqCounter[seq] = seqCounter.getOrDefault(seq, 0L) + bananas[i + 4]
                }
            }
        }

        return seqCounter.values.max().toString()
    }
}