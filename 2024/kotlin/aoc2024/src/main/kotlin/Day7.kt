package cz.dawnflash.aoc2024

class Day7 : Day() {
    override val sampleChecks = "3749" to "11387"
    override val checks = "2654749936343" to "124060392153684"

    enum class Operator { ADD, MUL, CAT }

    private fun isSolvable(
        eqn: Pair<ULong, List<ULong>>,
        allowCat: Boolean,
        fixed: List<Operator> = listOf()
    ): Boolean {
        if (fixed.size == eqn.second.size - 1) {
            return eqn.first == eqn.second.reduceIndexed { i, acc, r ->
                when (fixed[i - 1]) {
                    Operator.ADD -> acc + r
                    Operator.MUL -> acc * r
                    Operator.CAT -> "${acc}${r}".toULong()
                }
            }
        }
        val base = isSolvable(eqn, allowCat, fixed + Operator.ADD) || isSolvable(eqn, allowCat, fixed + Operator.MUL)
        if (allowCat && !base) return isSolvable(eqn, true, fixed + Operator.CAT)
        return base
    }

    override fun solution1(input: List<String>): String {
        val parsed = input.map { line ->
            line.split(": ").let { it[0].toULong() to it[1].split(' ').map(String::toULong) }
        }
        return parsed.filter { isSolvable(it, false) }.sumOf { it.first }.toString()
    }

    override fun solution2(input: List<String>): String {
        val parsed = input.map { line ->
            line.split(": ").let { it[0].toULong() to it[1].split(' ').map(String::toULong) }
        }
        return parsed.filter { isSolvable(it, true) }.sumOf { it.first }.toString()
    }
}