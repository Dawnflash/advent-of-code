package cz.dawnflash.aoc2024

import java.io.File
import kotlin.time.measureTimedValue


open class Day {
    open val sampleChecks = "" to ""
    open val checks = "" to ""
    open fun solution1(input: List<String>) = "???"
    open fun solution2(input: List<String>) = "???"
    private fun solvePart(part: Int, inputFile: File, sampleFile: File) {
        val sampleCheck = if (part == 1) sampleChecks.first else sampleChecks.second
        val check = if (part == 1) checks.first else checks.second
        println("--${part}--")
        if (sampleFile.exists()) {
            val input = sampleFile.readText().lines().dropLastWhile { it.isEmpty() }
            val result = measureTimedValue { if (part == 1) solution1(input) else solution2(input) }
            val test = when (sampleCheck) {
                "" -> "DONE"
                result.value -> "PASS"
                else -> "FAIL"
            }
            println("Sample result [${test} in ${result.duration}]: ${result.value}")
        }
        if (inputFile.exists()) {
            val input = inputFile.readText().lines().dropLastWhile { it.isEmpty() }
            val result = measureTimedValue { if (part == 1) solution1(input) else solution2(input) }
            val test = when (check) {
                "" -> "DONE"
                result.value -> "PASS"
                else -> "FAIL"
            }
            println("Result [${test} in ${result.duration}]: ${result.value}")
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