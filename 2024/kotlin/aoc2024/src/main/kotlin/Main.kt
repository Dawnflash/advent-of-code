package cz.dawnflash.aoc2024

val days: Array<Day> = arrayOf(Day1(), Day2(), Day3(), Day4(), Day5(), Day6(), Day7(), Day8(), Day9(), Day10(), Day11(), Day12(), Day13(), Day14(), Day15(), Day16(), Day17(), Day18(), Day19(), Day20(), Day21(), Day22(), Day23(), Day24(), Day25())

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        for (day in days) {
            day.solve()
        }
    } else {
        val n = args[0].toIntOrNull()
        if (n != null && n > 0 && n < 26) {
            days[n - 1].solve()
        } else {
            println("Enter a valid day 1-25")
        }
    }
}