package cz.dawnflash.aoc2024

class Day3 : Day() {
    override val sampleChecks = "161" to "48"
    override val checks = "173529487" to "99532691"

    override fun solution1(input: List<String>) = solution(input.joinToString(""))
    override fun solution2(input: List<String>) = solution(
        input.joinToString("")
            .replace(Regex("""don't\(\).*?do\(\)"""), "")
    )

    private fun solution(input: String) = Regex("""mul\((\d+),(\d+)\)""").findAll(input)
        .sumOf { it.groupValues[1].toInt() * it.groupValues[2].toInt() }.toString()
}