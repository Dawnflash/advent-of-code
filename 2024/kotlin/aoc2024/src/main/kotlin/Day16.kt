package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Direction
import cz.dawnflash.aoc2024.util.Map2D
import cz.dawnflash.aoc2024.util.Point
import cz.dawnflash.aoc2024.util.step

class Day16 : Day() {
    override val sampleChecks = "7036" to "45"
    override val checks = "65436" to "489"

    data class Maze(val map: Map2D<Boolean>) {
        val start = 1 to map.h - 2
        val end = map.w - 2 to 1
        val shortest: HashMap<Point, HashMap<Direction, Int>> = hashMapOf(start to hashMapOf(Direction.E to 0))
    }

    private fun parse(input: List<String>): Maze {
        val map = input.map { line -> line.map { it == '#' } }
        return Maze(Map2D.from(map))
    }

    private fun findShortestPath(maze: Maze) {
        val stack: MutableList<Pair<Point, Direction>> = mutableListOf()
        stack.add(maze.start to Direction.E)
        while (stack.isNotEmpty()) {
            val (cur, curDir) = stack.removeLast()
            val nextDirs = listOf(curDir to 0, curDir.turnLeft() to 1000, curDir.turnRight() to 1000)
            for ((dir, penalty) in nextDirs) {
                val next = cur.step(dir)
                val pathLen = 1 + penalty + maze.shortest[cur]!![curDir]!!
                if (maze.map.at(next)) continue
                val bestLen = maze.shortest[next]?.get(dir)
                if (bestLen == null || bestLen > pathLen) {
                    maze.shortest.getOrPut(next) { HashMap() }[dir] = pathLen

                    if (next != maze.end) stack.add(next to dir)
                }
            }
        }
    }

    private fun optimalSpots(
        maze: Maze,
        cur: Point = maze.end,
        egressDir: Direction? = null,
        points: MutableSet<Point> = mutableSetOf()
    ): Set<Point> {
        points.add(cur)
        if (cur == maze.start) return points
        val options = maze.shortest[cur]!!
        if (egressDir == null) {
            val min = options.values.min()
            options.filter { it.value == min }.keys.forEach {
                optimalSpots(maze, cur.step(it.turnAround()), it, points)
            }
        } else {
            val adjOptions = options.map {
                it.key to when (it.key) {
                    egressDir -> it.value
                    else -> it.value + 1000
                }
            }
            val min = adjOptions.minOfOrNull { it.second }
            adjOptions.filter { it.second == min }.forEach {
                optimalSpots(maze, cur.step(it.first.turnAround()), it.first, points)
            }
        }
        return points
    }

    private fun printMaze(maze: Maze) {
        val optPath = optimalSpots(maze)
        val map = maze.map.data.mapIndexed { y, line ->
            line.mapIndexed { x, it ->
                when {
                    it -> '#'
                    optPath.contains(x to y) -> 'O'
                    else -> '.'
                }
            }
        }
        Map2D.from(map).print()
    }

    override fun solution1(input: List<String>): String {
        val maze = parse(input)
        findShortestPath(maze)
        return maze.shortest[maze.end]!!.values.min().toString()
    }

    override fun solution2(input: List<String>): String {
        val maze = parse(input)
        findShortestPath(maze)
        return optimalSpots(maze).size.toString()
    }
}