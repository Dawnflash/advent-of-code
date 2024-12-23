package cz.dawnflash.aoc2024

import cz.dawnflash.aoc2024.util.Graph
import cz.dawnflash.aoc2024.util.combinations2

class Day23 : Day() {
    override val sampleChecks = "7" to ""
    override val checks = "" to ""

    fun parse(input: List<String>): Graph<String> {
        val edges = HashMap<String, MutableList<String>>()
        input.map { line ->
            line.split("-").let { verts ->
                verts.forEach { v -> edges.putIfAbsent(v, mutableListOf()) }
                edges[verts[0]]!!.add(verts[1])
                edges[verts[1]]!!.add(verts[0])
            }
        }
        return Graph.fromUnweighted(edges)
    }

    private fun getTriangles(graph: Graph<String>): Set<Set<String>> {
        val triangles = mutableSetOf<Set<String>>()
        for ((v, neighs) in graph.edges) {
            if (neighs.size < 2) continue
            for ((a, b) in neighs.keys.combinations2()) {
                if (graph.hasEdge(a, b)) triangles.add(setOf(v, a, b))
            }
        }

        return triangles
    }

    // returns larger cliques
    private fun expandClique(graph: Graph<String>, old: Set<Set<String>>): Set<Set<String>> {
        val new = mutableSetOf<Set<String>>()
        for (clique in old) {
            for (c in clique.map { graph.edges[it]!!.keys }.reduce { a, b -> a.intersect(b) }) {
                if (clique.all { graph.hasEdge(c, it) }) new.add(clique + c)
            }
        }
        return new
    }

    override fun solution1(input: List<String>): String {
        val graph = parse(input)
        val triangles = getTriangles(graph)
        return triangles.filter { t -> t.any { it.startsWith("t") } }.size.toString()
    }

    override fun solution2(input: List<String>): String {
        val graph = parse(input)
        var clique = getTriangles(graph)
        var best = setOf<String>()
        while (clique.isNotEmpty()) {
            best = clique.take(1)[0]
            clique = expandClique(graph, clique)
        }

        return best.toList().sorted().joinToString(",")
    }
}