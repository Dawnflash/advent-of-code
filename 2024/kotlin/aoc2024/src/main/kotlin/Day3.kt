package cz.dawnflash.aoc2024

class Day3 : Day() {
    override val sampleChecks = "161" to "48"
    override val checks = "173529487" to "99532691"

    override fun solution1(input: List<String>): String {
        val regex = Regex("""mul\((\d+),(\d+)\)""")
        return regex.findAll(input.joinToString("")).sumOf { it.groupValues[1].toInt() * it.groupValues[2].toInt() }.toString()
    }
    override fun solution2(input: List<String>): String {
        val dropRegex = Regex("""don't\(\).*?do\(\)""")
        return solution1(input.joinToString("").replace(dropRegex, "").lines())
    }
}