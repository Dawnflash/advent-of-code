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
        val bananas = input.map { l -> (1..2000).runningFold(l.toLong()) { acc, _ -> gen(acc) }.map { it % 10 } }
        val diffs = bananas.map { gen -> gen.windowed(2).map { it[1] - it[0] } }
        val visited = hashSetOf<List<Long>>()
        val seqMaps: List<HashMap<List<Long>, Long>> = diffs.mapIndexed { m, diff ->
            visited.clear()
            HashMap(diff.windowed(4).mapIndexedNotNull { i, seq ->
                if (seq !in visited) {
                    visited.add(seq)
                    seq to bananas[m][i + 4]
                } else null
            }.toMap())
        }
        visited.clear()
        val best = seqMaps.mapNotNull { monkey ->
            monkey.keys.mapNotNull { seq ->
                if (seq in visited) {
                    null
                } else {
                    visited.add(seq)
                    seqMaps.sumOf { it.getOrDefault(seq, 0L) }
                }
            }.maxOrNull()
        }.maxOrNull()

        return best.toString()
    }
}