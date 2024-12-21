package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Direction

class Day21 : Day() {
    override val sampleChecks = "126384" to ""
    override val checks = "125742" to "157055032722640"

    private abstract class Keypad {
        val start = 'A'
        protected abstract val mapping: Map<Char, Map<Direction, Char>>
        protected val invMapping: Map<Char, Map<Char, Direction>> by lazy {
            this.mapping.map { (c, m) ->
                c to m.map { (k, v) -> v to k }.toMap()
            }.toMap()
        }

        // can skip adjacent entries
        protected abstract fun nextDir(cur: Char, goal: Char): List<Direction>

        fun next(cur: Char, goal: Char): List<Pair<Char, Direction>> {
            val im = invMapping[cur]!![goal]
            if (im != null) return listOf(goal to im)

            val dirs = nextDir(cur, goal)

            return dirs.map { mapping[cur]!![it]!! to it }
        }

        fun paths(start: Char, end: Char): List<String> {
            if (start == end) return listOf("${this.start}")

            return next(start, end).flatMap { (nc, d) ->
                paths(nc, end).map { "$d$it" }
            }
        }

        fun translate(input: String, depth: Int, memo: HashMap<Pair<String, Int>, ULong> = hashMapOf()): ULong {
            if (depth == 0) return input.length.toULong()
            val m = memo[input to depth]
            if (m != null) return m
            return "$start$input".windowed(2, 1).sumOf { w ->
                val dir = DirectionalKeypad()
                paths(w[0], w[1]).minOfOrNull { dir.translate(it, depth - 1, memo) }!!
            }.also { memo[input to depth] = it }
        }
    }

    private class DirectionalKeypad : Keypad() {
        override val mapping = mapOf(
            'A' to mapOf(
                Direction.S to '>',
                Direction.W to '^'
            ),
            '^' to mapOf(
                Direction.S to 'v',
                Direction.E to 'A',
            ),
            'v' to mapOf(
                Direction.E to '>',
                Direction.N to '^',
                Direction.W to '<',
            ),
            '<' to mapOf(
                Direction.E to 'v',
            ),
            '>' to mapOf(
                Direction.N to 'A',
                Direction.W to 'v',
            ),
        )

        override fun nextDir(cur: Char, goal: Char): List<Direction> = when (cur) {
            'A' -> listOf(Direction.S, Direction.W)
            '>' -> when (goal) {
                '<' -> listOf(Direction.W)
                else -> listOf(Direction.N, Direction.W) // > -> ^
            }
            'v' -> listOf(Direction.E, Direction.N)
            '<' -> listOf(Direction.E)
            else -> when(goal) { // ^
                '<' -> listOf(Direction.S)
                else -> listOf(Direction.S, Direction.E) // ^ -> >
            }
        }
    }

    private class NumericKeypad : Keypad() {
        override val mapping = mapOf(
            'A' to mapOf(
                Direction.W to '0',
                Direction.N to '3'
            ),
            '0' to mapOf(
                Direction.E to 'A',
                Direction.N to '2',
            ),
            '1' to mapOf(
                Direction.E to '2',
                Direction.N to '4',
            ),
            '2' to mapOf(
                Direction.E to '3',
                Direction.N to '5',
                Direction.W to '1',
                Direction.S to '0',
            ),
            '3' to mapOf(
                Direction.N to '6',
                Direction.W to '2',
                Direction.S to 'A',
            ),
            '4' to mapOf(
                Direction.E to '5',
                Direction.N to '7',
                Direction.S to '1',
            ),
            '5' to mapOf(
                Direction.E to '6',
                Direction.N to '8',
                Direction.W to '4',
                Direction.S to '2',
            ),
            '6' to mapOf(
                Direction.N to '9',
                Direction.W to '5',
                Direction.S to '3',
            ),
            '7' to mapOf(
                Direction.E to '8',
                Direction.S to '4',
            ),
            '8' to mapOf(
                Direction.E to '9',
                Direction.W to '7',
                Direction.S to '5',
            ),
            '9' to mapOf(
                Direction.W to '8',
                Direction.S to '6',
            ),
        )

        override fun nextDir(cur: Char, goal: Char): List<Direction> = when (cur) {
            'A' -> when (goal) {
                '6', '9' -> listOf(Direction.N)
                else -> listOf(Direction.N, Direction.W)
            }

            '0' -> when (goal) {
                '3', '6', '9' -> listOf(Direction.N, Direction.E)
                else -> listOf(Direction.N)
            }

            '1' -> when (goal) {
                '7' -> listOf(Direction.N)
                '3', '0', 'A' -> listOf(Direction.E)
                else -> listOf(Direction.N, Direction.E)
            }

            '2' -> when (goal) {
                'A' -> listOf(Direction.S, Direction.E)
                '8' -> listOf(Direction.N)
                '7', '4' -> listOf(Direction.N, Direction.W)
                else -> listOf(Direction.N, Direction.E)
            }

            '3' -> when (goal) {
                '1' -> listOf(Direction.W)
                '0' -> listOf(Direction.S, Direction.W)
                '9' -> listOf(Direction.N)
                else -> listOf(Direction.N, Direction.W)
            }

            '4' -> when (goal) {
                '8', '9' -> listOf(Direction.N, Direction.E)
                '6' -> listOf(Direction.E)
                else -> listOf(Direction.S, Direction.E)
            }

            '5' -> when (goal) {
                '0' -> listOf(Direction.S)
                '7' -> listOf(Direction.N, Direction.W)
                '9' -> listOf(Direction.N, Direction.E)
                '1' -> listOf(Direction.S, Direction.W)
                else -> listOf(Direction.S, Direction.E)
            }

            '6' -> when (goal) {
                '7', '8' -> listOf(Direction.N, Direction.W)
                '4' -> listOf(Direction.W)
                'A' -> listOf(Direction.S)
                else -> listOf(Direction.S, Direction.W)
            }

            '7' -> when (goal) {
                '9' -> listOf(Direction.E)
                '1' -> listOf(Direction.S)
                else -> listOf(Direction.S, Direction.E)
            }

            '8' -> when (goal) {
                '2', '0' -> listOf(Direction.S)
                '4', '1' -> listOf(Direction.S, Direction.W)
                else -> listOf(Direction.S, Direction.E)
            }

            '9' -> when (goal) {
                '7' -> listOf(Direction.W)
                '3', 'A' -> listOf(Direction.S)
                else -> listOf(Direction.S, Direction.W)
            }

            else -> error("unreachable")
        }
    }

    override fun solution1(input: List<String>): String {
        val num = NumericKeypad()
        return input.sumOf { key ->
            num.translate(key, 3) * key.replace("A", "").toULong()
        }.toString()
    }

    override fun solution2(input: List<String>): String {
        val num = NumericKeypad()
        return input.sumOf { key ->
            num.translate(key, 26) * key.replace("A", "").toULong()
        }.toString()
    }
}