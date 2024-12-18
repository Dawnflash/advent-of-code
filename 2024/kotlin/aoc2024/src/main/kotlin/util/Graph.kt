package cz.dawnflash.aoc2024.util

import java.util.*
import kotlin.collections.HashMap

// represents a directed weighted graph G=(V,E) with vertices of type V and edges with weight type W
// edges are stored as a neighbor hashmap
class Graph<V>(val vertices: Set<V>, val edges: Map<V, List<Pair<V, Double>>>) {
    fun shortestPathLength(start: V, end: V, estimator: (V) -> Double): Double? {
        val distances = HashMap<V, Double>(vertices.associateWith { Double.MAX_VALUE })
        val heap = PriorityQueue<V> { a, b ->
            distances[a]!!.compareTo(distances[b]!!)
        }
        val visited = mutableSetOf<V>()
        heap.add(start)
        distances[start] = 0.0
        while (heap.isNotEmpty()) {
            val v = heap.remove()
            val vBest = distances[v]!!
            if (v == end) {
                return vBest
            }
            visited.add(v)
            for ((neigh, weight) in edges[v]!!) {
                if (visited.contains(neigh)) continue

                val neighBest = distances[neigh]!!
                val calcBest = vBest + weight
                if (calcBest < neighBest) {
                    distances[neigh] = calcBest
                    heap.add(neigh)
                }
            }
        }
        return null // no path
    }
}