package cz.dawnflash.aoc2024

class Day5 : Day() {
    override val sampleChecks = "143" to "123"
    override val checks = "4959" to "4655"

    private fun parse(input: List<String>): Pair<List<List<Int>>, List<List<Int>>> {
        val splitter = input.indexOf("")
        val rules = input.slice(0..<splitter).map { it.split('|').map(String::toInt) }
        val updates = input.slice(splitter + 1..<input.size).map { it.split(',').map(String::toInt) }
        return rules to updates
    }

    private fun isValid(rule: List<Int>, job: List<Int>): Boolean {
        val indices = rule.map { job.indexOf(it) }
        return indices.any { it == -1 } || indices[0] < indices[1]
    }

    private fun fix(rules: List<List<Int>>, job: List<Int>): List<Int> {
        val applyingRules = rules.filter { rule -> job.any { rule[0] == it || rule[1] == it } }
        return job.sortedWith { a, b ->
            when {
                applyingRules.any { it[0] == a && it[1] == b } -> -1
                applyingRules.any { it[1] == a && it[0] == b } -> 1
                else -> 0
            }
        }
    }

    override fun solution1(input: List<String>): String {
        val (rules, updates) = parse(input)
        return updates.filter { rules.all { rule -> isValid(rule, it) } }.sumOf { it[it.size / 2] }.toString()
    }

    override fun solution2(input: List<String>): String {
        val (rules, updates) = parse(input)
        return updates.filter { rules.any { rule -> !isValid(rule, it) } }.sumOf { fix(rules, it)[it.size / 2] }.toString()
    }
}