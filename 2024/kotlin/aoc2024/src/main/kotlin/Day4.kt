package cz.dawnflash.aoc2024

import kotlin.math.min
import cz.dawnflash.aoc2024.util.transpose

class Day4 : Day() {
    override val sampleChecks = "18" to "9"
    override val checks = "2297" to "1745"

    private val needle = Regex("XMAS")

    private fun matches(input: String) =
        needle.findAll(input).count() + needle.findAll(input.reversed()).count()

    private fun diagonalsPrimary(width: Int, length: Int) =
        (1..<length).reversed().map { y ->
            (0..<min(min(width, length), length - y)).map { it to y + it }
        } + (0..<width).map { x ->
            (0..<min(min(width, length), width - x)).map { x + it to it }
        }

    private fun diagonalsSecondary(width: Int, length: Int) =
        (0..<width).map { x ->
            (0..<min(min(width, length), x + 1)).map { x - it to it }
        } + (1..<length).map { y ->
            (0..<min(min(width, length), length - y)).map { width - 1 - it to y + it }
        }

    private fun diagonals(width: Int, length: Int) =
        diagonalsPrimary(width, length) + diagonalsSecondary(width, length)

    private fun isMAS(input: List<String>, x: Int, y: Int) = input[y + 1][x + 1] == 'A' && (
            (input[y][x] == 'M' && input[y + 2][x + 2] == 'S' || input[y][x] == 'S' && input[y + 2][x + 2] == 'M') &&
                    (input[y + 2][x] == 'M' && input[y][x + 2] == 'S' || input[y + 2][x] == 'S' && input[y][x + 2] == 'M'))

    override fun solution1(input: List<String>): String {
        val (width, length) = input[0].length to input.size
        val hor = input.sumOf(this::matches)
        val ver = input.transpose().sumOf(this::matches)
        val dia = diagonals(width, length).sumOf {
            it.map { (x, y) -> input[y][x] }.joinToString("").let(this::matches)
        }
        return (hor + ver + dia).toString()
    }

    override fun solution2(input: List<String>): String {
        val (width, length) = input[0].length to input.size
        return (0..<length - 2)
            .sumOf { y -> (0..<width - 2).count { x -> isMAS(input, x, y) } }
            .toString()
    }
}
