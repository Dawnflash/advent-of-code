package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.Day7.Operator

class Day7 : Day() {
    override val sampleChecks = "3749" to "11387"
    override val checks = "2654749936343" to "124060392153684"

    enum class Operator { ADD, MUL, CAT }

    private fun isSolvable(
        eqn: Pair<ULong, List<ULong>>,
        ops: Array<Operator>,
        idx: Int = 1,
        acc: ULong = eqn.second[0],
    ): Boolean {
        if (idx == eqn.second.size) return acc == eqn.first
        val rhs = eqn.second[idx]
        return ops.any {
            isSolvable(eqn, ops, idx + 1, when (it) {
                Operator.ADD -> acc + rhs
                Operator.MUL -> acc * rhs
                Operator.CAT -> "$acc$rhs".toULong()
            })
        }
    }

    private fun solution(input: List<String>, ops: Array<Operator>): String {
        val parsed = input.map { line ->
            line.split(": ").let { it[0].toULong() to it[1].split(' ').map(String::toULong) }
        }
        return parsed.filter { isSolvable(it, ops) }.sumOf { it.first }.toString()
    }

    override fun solution1(input: List<String>) = solution(input, arrayOf(Operator.ADD, Operator.MUL))
    override fun solution2(input: List<String>) = solution(input, arrayOf(Operator.ADD, Operator.MUL, Operator.CAT))
}