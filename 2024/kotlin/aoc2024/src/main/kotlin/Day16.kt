package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.*
import java.util.PriorityQueue

class Day16 : Day() {
    override val sampleChecks = "7036" to "45"
    override val checks = "65436" to "489"

    data class Maze(val map: Map2D<Boolean>) {
        val start = 1 to map.h - 2
        val end = map.w - 2 to 1
        val graph: HashMap<Point, HashMap<Direction, Triple<Direction, Point, Int>>> = hashMapOf()
        val shortest: HashMap<Point, HashMap<Direction, Int>> = hashMapOf(start to hashMapOf(Direction.E to 0))

        fun next(p: Point, d: Direction) = listOf(d to 0, d.turnLeft() to 1000, d.turnRight() to 1000).filter {
            !map.at(p.step(it.first))
        }

        fun distFromOrigin(p: Point) = shortest[p]?.minOf { it.value }
        fun estimatedPathLen(p: Point) = when(val d = distFromOrigin(p)) {
            null -> Int.MAX_VALUE
            else -> d + p.distanceFrom(end)
        }
    }

    private fun parse(input: List<String>): Maze {
        val map = input.map { line -> line.map { it == '#' } }
        return Maze(Map2D.from(map))
    }

    private fun buildGraph(maze: Maze, node: Point = maze.start, nodeDir: Direction = Direction.E) {
        if (node == maze.end) return
        for ((dir, _) in maze.next(node, nodeDir)) {
            if (maze.graph[node]?.contains(dir) == true) continue
            var cur = node.step(dir)
            var curDir = dir
            var dist = 1
            while (true) {
                val nextDirs = maze.next(cur, curDir)
                when {
                    cur == maze.end || nextDirs.size > 1 -> {
                        // println("$node $nodeDir -> $cur $curDir | $dist")
                        maze.graph.getOrPut(node) { HashMap() }[dir] = Triple(curDir, cur, dist)
                        maze.graph.getOrPut(cur) { HashMap() }[curDir.turnAround()] =
                            Triple(dir.turnAround(), node, dist)
                        buildGraph(maze, cur, curDir)
                        break
                    }

                    nextDirs.isEmpty() -> break
                    else -> {
                        curDir = nextDirs[0].first
                        dist += 1 + nextDirs[0].second
                        cur = cur.step(curDir)
                    }
                }
            }
        }
    }

    private fun findShortestPath(maze: Maze) {
        val heap: PriorityQueue<Pair<Point, Direction>> = PriorityQueue { a, b ->
            maze.estimatedPathLen(a.first) - maze.estimatedPathLen(b.first)
        }
        heap.add(maze.start to Direction.E)
        while (heap.isNotEmpty()) {
            val (cur, curDir) = heap.remove()
            val neighbors = maze.graph[cur]!!
            for ((dirOut, penalty) in maze.next(cur, curDir)) {
                val (dirIn, next, nextLen) = neighbors[dirOut] ?: continue
                val pathLen = penalty + nextLen + maze.shortest[cur]!![curDir]!!
                val bestLen = maze.shortest[next]?.get(dirIn)
                if (bestLen == null || bestLen > pathLen) {
                    maze.shortest.getOrPut(next) { HashMap() }[dirIn] = pathLen

                    if (next != maze.end) heap.add(next to dirIn)
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
        val options = maze.shortest[cur]!!.map {
            it.key to when {
                egressDir == null || it.key == egressDir -> it.value
                else -> it.value + 1000 // add turn penalty
            }
        }
        val min = options.minOfOrNull { it.second }
        options.filter { it.second == min }.forEach {
            var dir = it.first.turnAround()
            var next = cur.step(dir)
            while (!maze.shortest.contains(next)) {
                points.add(next)
                dir = maze.next(next, dir)[0].first
                next = next.step(dir)
            }
            optimalSpots(maze, next, dir.turnAround(), points)
        }
        return points
    }

    override fun solution1(input: List<String>): String {
        val maze = parse(input)
        buildGraph(maze)
        findShortestPath(maze)
        return maze.distFromOrigin(maze.end).toString()
    }

    override fun solution2(input: List<String>): String {
        val maze = parse(input)
        buildGraph(maze)
        findShortestPath(maze)
        return optimalSpots(maze).size.toString()
    }
}