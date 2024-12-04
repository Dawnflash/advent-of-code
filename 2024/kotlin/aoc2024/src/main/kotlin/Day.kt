package cz.dawnflash.aoc2024

import java.io.File
import kotlin.time.measureTimedValue

open class Day {
    open val sampleChecks = "" to ""
    open val checks = "" to ""
    open val notChecks: Pair<Array<String>, Array<String>> = arrayOf<String>() to arrayOf()
    open val intervalChecks: Pair<Pair<String, String>, Pair<String, String>> = ("" to "") to ("" to "")

    open fun solution1(input: List<String>) = "???"
    open fun solution2(input: List<String>) = "???"
    private fun solvePart(part: Int, inputFile: File, sampleFile: File) {
        val sampleCheck = sampleChecks.toList()[part - 1]
        val check = checks.toList()[part - 1]
        val notCheck = notChecks.toList()[part - 1]
        val intervalCheck = intervalChecks.toList()[part - 1]
        val (intMin, intMax) = intervalCheck.toList().map { it.toBigIntegerOrNull() }

        println("--$part--")
        if (sampleFile.exists()) {
            val input = sampleFile.readText().lines().dropLastWhile { it.isEmpty() }
            val result = measureTimedValue { if (part == 1) solution1(input) else solution2(input) }
            val test = when (sampleCheck) {
                "" -> "DONE"
                result.value -> "PASS"
                else -> "FAIL ($sampleCheck)"
            }
            println("Sample result [$test in ${result.duration}]: ${result.value}")
        }
        if (inputFile.exists()) {
            val input = inputFile.readText().lines().dropLastWhile { it.isEmpty() }
            val result = measureTimedValue { if (part == 1) solution1(input) else solution2(input) }
            val resultNum = result.value.toBigIntegerOrNull()
            val test = when (check) {
                "" -> when {
                    notCheck.contains(result.value) -> "FAIL (!= ${result.value})"
                    resultNum != null && intMin != null && intMin >= resultNum -> "FAIL (> $intMin)"
                    resultNum != null && intMax != null && intMax <= resultNum -> "FAIL (< $intMax)"
                    else -> "DONE"
                }

                result.value -> "PASS"
                else -> "FAIL (== $check)"
            }
            println("Result [$test in ${result.duration}]: ${result.value}")
        } else {
            println("! Main input file not found")
        }
    }

    fun solve() {
        val name = this::class.simpleName
        val inputFile = File(ClassLoader.getSystemResource("${name}.txt")?.file ?: "")
        val sampleFile = File(ClassLoader.getSystemResource("${name}_sample.txt")?.file ?: "")

        println(name)
        solvePart(1, inputFile, sampleFile)
        solvePart(2, inputFile, sampleFile)
    }
}