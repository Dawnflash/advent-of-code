package cz.dawnflash.aoc2024

class Day17 : Day() {
    override val sampleChecks = "5,7,3,0" to "117440"
    override val checks = "4,1,7,6,4,1,0,2,7" to "164279024971453"

    private class CPU(var a: Long, var b: Long, var c: Long) {
        fun combo(operand: Int) = when {
            operand <= 3 -> operand.toLong()
            operand == 4 -> a
            operand == 5 -> b
            operand == 6 -> c
            else -> error("unknown combo operand")
        }

        fun run(instructions: List<Pair<Int, Int>>, newA: Long? = null): List<Int> {
            val (aa, bb, cc) = Triple(a, b, c)
            if (newA != null) a = newA
            val out = mutableListOf<Int>()
            var cir = 0
            while (cir < instructions.size) {
                val (opcode, operand) = instructions[cir++]
                when (opcode) {
                    0 -> a = a.shr(combo(operand).toInt()) // adv
                    1 -> b = b.xor(operand.toLong()) // bxl
                    2 -> b = combo(operand) % 8 // bst
                    3 -> if (a != 0L) cir = operand
                    4 -> b = b.xor(c) // bxc
                    5 -> out.add((combo(operand) % 8).toInt()) // out
                    6 -> b = a.shr(combo(operand).toInt()) // bdv
                    7 -> c = a.shr(combo(operand).toInt()) // cdv
                    else -> error("unknown opcode")
                }
            }
            a = aa
            b = bb
            c = cc
            return out
        }
    }

    private fun parse(input: List<String>): Pair<CPU, List<Pair<Int, Int>>> {
        val cpu = input.subList(0, 3).map { line ->
            line.split(": ")[1].toLong()
        }.let {
            CPU(it[0], it[1], it[2])
        }
        val program = input[4].split(": ")[1].split(",").map {
            it.toInt()
        }.windowed(2, 2).map { it[0] to it[1] }
        return cpu to program
    }

    private fun search(c: CPU, program: List<Pair<Int, Int>>): Long {
        val nProgram = program.flatMap { listOf(it.first, it.second) }
        var a = 0L
        var out: List<Int>
        for (depth in nProgram.indices) {
            out = c.run(program, a)
            while (out.size < depth + 1
                || nProgram.subList(nProgram.size - 1 - depth, nProgram.size) != out.subList(
                    out.size - 1 - depth,
                    out.size
                )
            ) {
                a++
                out = c.run(program, a)
            }
            if (depth < nProgram.size - 1) a = a.shl(3)
        }
        assert(c.run(program, a) == nProgram)
        return a
    }

    override fun solution1(input: List<String>): String {
        val (cpu, program) = parse(input)
        return cpu.run(program).joinToString(",")
    }

    override fun solution2(input: List<String>): String {
        val (cpu, program) = parse(input)
        return search(cpu, program).toString()
    }
}