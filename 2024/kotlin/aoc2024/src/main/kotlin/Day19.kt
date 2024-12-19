package cz.dawnflash.aoc2024

class Day19 : Day() {
    override val sampleChecks = "6" to "16"
    override val checks = "260" to "639963796864990"

    private fun possible(pattern: String, towels: Set<String>, memo: HashMap<String, Long> = hashMapOf()): Long {
        if (pattern.isEmpty()) return 1
        return memo.getOrPut(pattern) {
            pattern.indices.sumOf { i ->
                if (towels.contains(pattern.substring(0, i + 1))) {
                    possible(pattern.substring(i + 1, pattern.length), towels, memo)
                } else 0
            }
        }
    }

    override fun solution1(input: List<String>): String {
        val towels = input[0].split(", ").toSet()
        val desired = input.drop(2)
        return desired.count { possible(it, towels) > 0 }.toString()
    }

    override fun solution2(input: List<String>): String {
        val towels = input[0].split(", ").toSet()
        val desired = input.drop(2)
        return desired.sumOf { possible(it, towels) }.toString()
    }
}