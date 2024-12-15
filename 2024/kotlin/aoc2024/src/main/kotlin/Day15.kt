package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Direction
import cz.dawnflash.aoc2024.util.Map2D
import cz.dawnflash.aoc2024.util.Point
import cz.dawnflash.aoc2024.util.step

class Day15 : Day() {
    override val sampleChecks = "10092" to "9021"
    override val checks = "1463715" to "1481392"

    private enum class Block(val c: Char) {
        Wall('#'), Box('O'), Empty('.'), LBox('['), RBox(']'), Robot('@');

        override fun toString() = c.toString()

        companion object {
            fun from(c: Char) = Block.entries.find { it.c == c }!!
        }
    }

    private fun move(
        map: Map2D<Block>, pos: Point, dir: Direction, dryRun: Boolean = false
    ): Point {
        val nPos = pos.step(dir)
        return when (val next = map.at(nPos)) {
            Block.Wall -> pos
            Block.Empty -> nPos.also {
                if (!dryRun) {
                    map.set(nPos, map.at(pos))
                    map.set(pos, Block.Empty)
                }
            }

            Block.Box -> if (move(map, nPos, dir, dryRun) == nPos) pos else {
                if (dryRun) nPos else move(map, pos, dir)
            }

            Block.LBox, Block.RBox -> {
                // check if the box half can be moved
                if (move(map, nPos, dir, true) == nPos) return pos
                if (dir == Direction.N || dir == Direction.S) {
                    // in vertical moves the box halves move atomically
                    val other = nPos.step(if (next == Block.LBox) Direction.E else Direction.W)
                    // move the paired box half
                    if (move(map, other, dir, dryRun) == other) pos else {
                        if (dryRun) nPos else {
                            move(map, nPos, dir) // complete the original box half move
                            move(map, pos, dir)
                        }
                    }
                } else {
                    if (dryRun) nPos else {
                        move(map, nPos, dir) // complete the original box half move
                        move(map, pos, dir)
                    }
                }
            }

            else -> error("invalid state")
        }
    }

    private fun parse(input: List<String>): Triple<Map2D<Block>, List<Direction>, Point> {
        val split = input.indexOf("")
        val map = Map2D.from(input.subList(0, split).map { line ->
            line.map { Block.from(it) }
        })
        val directions = input.subList(split + 1, input.size).flatMap { line ->
            line.map { Direction.fromChar(it) }
        }
        return Triple(map, directions, map.findAll { it == Block.Robot }[0])
    }

    private fun expand(map: Map2D<Block>): Map2D<Block> = Map2D.from(map.data.map { line ->
        line.flatMap {
            when (it) {
                Block.Box -> listOf(Block.LBox, Block.RBox)
                Block.Robot -> listOf(Block.Robot, Block.Empty)
                else -> listOf(it, it)
            }
        }
    })

    private fun score(map: Map2D<Block>) = map.data.flatMapIndexed { y, line ->
        line.mapIndexed { x, it ->
            when (it) {
                Block.LBox, Block.Box -> 100 * y + x
                else -> 0
            }
        }
    }.sum()

    override fun solution1(input: List<String>): String {
        val (map, directions, robotPos) = parse(input)
        directions.fold(robotPos) { acc, dir -> move(map, acc, dir) }

        return score(map).toString()
    }

    override fun solution2(input: List<String>): String {
        var (map, directions, robotPos) = parse(input)
        map = expand(map)
        robotPos = robotPos.first * 2 to robotPos.second
        directions.fold(robotPos) { acc, dir -> move(map, acc, dir) }

        return score(map).toString()
    }
}