package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.*

class Day12 : Day() {
    override val sampleChecks = "1930" to "1206"
    override val checks = "1304764" to "811148"

    private fun measureRegion(
        part2: Boolean,
        map: Map2D<Char>,
        visited: List<MutableList<Boolean>>,
        p: Point,
        region: Char = map.at(p),
    ): Pair<Int, Int> {
        if (visited.at(p)) return 0 to 0
        visited[p.second][p.first] = true
        val neighbors = map.neighbors4(p).filter { map.at(it) == region }
        val walls = Direction.cardinals.count { dir ->
            map.atSafe(p.step(dir)) != region && (!part2 || listOf(dir.turnRight(), dir.turnLeft()).all {
                var np = p.step(it)
                while (map.atSafe(np) == region && map.atSafe(np.step(dir)) != region) {
                    if (visited.at(np)) return@count false
                    np = np.step(it)
                }
                true
            })
        }
        val (area, perimeter) = neighbors.map { measureRegion(part2, map, visited, it, region) }
            .fold(0 to 0) { (a1, p1), (a2, p2) -> a1 + a2 to p1 + p2 }
        return area + 1 to perimeter + walls
    }

    override fun solution(input: List<String>, part2: Boolean): String {
        val map = Map2D(input.map { it.toCharArray().toList() })
        val visited = List(map.h) { MutableList(map.w) { false } }
        return (0..<map.h).sumOf { y ->
            (0..<map.w).sumOf { x ->
                val (area, perimeter) = measureRegion(part2, map, visited, x to y)
                area * perimeter
            }
        }.toString()
    }
}