package cz.dawnflash.aoc2024.util

import java.util.*
import kotlin.collections.HashMap

// represents a directed weighted graph G=(V,E) with vertices of type V
// edges are weighted by Doubles and stored as a neighbor hashmap
class Graph<V>(val vertices: HashSet<V>, val edges: HashMap<V, List<Pair<V, Double>>>) {
    companion object {
        fun <V> buildPath(from: Map<V, V>, start: V, end: V): List<V> {
            val path = mutableListOf(end)
            var cur = end
            while (cur != start) {
                cur = from[cur]!!
                path.add(cur)
            }
            return path
        }
    }

    fun shortestPath(start: V, end: V, estimator: (V) -> Double): Pair<Double, List<V>>? {
        val res = aStar(start, end, estimator) ?: return null
        return res.first[end]!! to buildPath(res.second, start, end)
    }

    fun distancesFrom(target: V) = aStar(target, null) { 0.0 }

    // A* pathfinding algorithm
    // short-circuits if end is provided otherwise returns all distances and from map
    private fun aStar(start: V, end: V?, estimator: (V) -> Double): Pair<Map<V, Double>, Map<V, V>>? {
        // g-score
        val distances = HashMap<V, Double>()
        // f-score = g-score + estimator
        val estimates = HashMap<V, Double>(vertices.associateWith { Double.MAX_VALUE })
        val from = HashMap<V, V>()
        val heap = PriorityQueue<V> { a, b ->
            estimates[a]!!.compareTo(estimates[b]!!)
        }
        val visited = mutableSetOf<V>()
        heap.add(start)
        distances[start] = 0.0
        estimates[start] = estimator(start)
        while (heap.isNotEmpty()) {
            val v = heap.remove()
            val vBest = distances[v]!!
            if (v == end) {
                return distances to from
            }
            visited.add(v)
            for ((neigh, weight) in edges[v]!!) {
                if (neigh in visited) continue

                val neighBest = distances[neigh] ?: Double.MAX_VALUE
                val calcBest = vBest + weight
                if (calcBest < neighBest) {
                    distances[neigh] = calcBest
                    estimates[neigh] = calcBest + estimator(neigh)
                    heap.add(neigh)
                    from[neigh] = v
                }
            }
        }
        if (end != null) return null // no path
        return distances to from
    }
}