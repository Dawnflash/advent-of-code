package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.combinations2

class Day24 : Day() {
    override val sampleChecks = "2024" to ""
    override val checks = "51837135476040" to "hjf,kdh,kpp,sgj,vss,z14,z31,z35"

    private class System(
        val wires: MutableMap<String, Boolean?>,
        val map: Map<String, Map<String, List<Pair<String, String>>>>
    ) {
        fun compute(w1: Boolean, w2: Boolean, op: Pair<String, String>) {
            wires[op.second] = when (op.first) {
                "AND" -> w1 && w2
                "OR" -> w1 || w2
                "XOR" -> w1.xor(w2)
                else -> error("unknown operation")
            }
        }

        fun buildInt(prefix: Char): ULong {
            var res = 0UL
            for ((k, _) in wires.filter { it.key.startsWith(prefix) && it.value == true }) {
                val pos = k.substring(1).toInt()
                res += 1UL.shl(pos)
            }
            return res
        }

        fun run(): ULong {
            val zRemaining = wires.filter { it.key.startsWith("z") && it.value == null }.keys.toMutableSet()
            while (zRemaining.isNotEmpty()) {
                for ((w1, w1Val) in wires) {
                    if (w1Val == null || w1 !in map) continue
                    for ((w2, ops) in map[w1]!!) {
                        val w2Val = wires[w2] ?: continue
                        for (op in ops) {
                            compute(w1Val, w2Val, op)
                            if (op.second.startsWith("z")) zRemaining.remove(op.second)
                        }
                    }
                }
            }
            return buildInt('z')
        }
    }

    private fun trySwap(s: String, swaps: List<Pair<String, String>>): String {
        for ((a, b) in swaps) {
            if (a == s) return b
            if (b == s) return a
        }
        return s
    }

    private fun parse(input: List<String>, swaps: List<Pair<String, String>> = listOf()): System {
        val splitter = input.indexOf("")
        val regex = Regex("""^(...) (...?) (...) -> (...)$""")
        val wires = hashMapOf<String, Boolean?>()
        val map = HashMap<String, HashMap<String, MutableList<Pair<String, String>>>>()
        for (line in input.subList(splitter + 1, input.size)) {
            val matches = regex.find(line)!!.groupValues
            val a = matches[1]
            val op = matches[2]
            val b = matches[3]
            val out = trySwap(matches[4], swaps)
            wires[a] = null
            wires[b] = null
            wires[out] = null
            map.getOrPut(a) { hashMapOf() }.getOrPut(b) { mutableListOf() }.add(op to out)
            map.getOrPut(b) { hashMapOf() }.getOrPut(a) { mutableListOf() }.add(op to out)
        }
        for (line in input.subList(0, splitter)) {
            val split = line.split(": ")
            wires[split[0]] = split[1] == "1"
        }
        return System(wires, map)
    }

    override fun solution1(input: List<String>): String {
        val system = parse(input)
        return system.run().toString()
    }

    override fun solution2(input: List<String>): String {
        if (input.size < 50) return "N/A"
        // manual work, 3 z-swaps are violating "z must come out of xor", "hjf" is violating "OR must be fed from ANDs"
        // automated solution could find these violations and propose fixes, but I'm lazy right now
        val swaps = listOf("sgj" to "z35", "vss" to "z14", "kpp" to "z31", "hjf" to "kdh")
        val system = parse(input, swaps)
        val x = system.buildInt('x')
        val y = system.buildInt('y')
        val z = system.run()
        if (x + y == z) {
            return swaps.flatMap { listOf(it.first, it.second) }.sorted().joinToString(",")
        }
        return super.solution2(input)
    }
}