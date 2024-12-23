package cz.dawnflash.aoc2024.util

fun <T> List<List<T>>.transpose(): List<List<T>> {
    return this[0].indices.map { i -> this.indices.map { j -> this[j][i] } }
}

@JvmName("listStringTranspose")
fun List<String>.transpose(): List<String> {
    return this.indices.map { j -> this[0].indices.map { i -> this[i][j] }.joinToString("") }
}

fun <T> List<T>.combinations2(): List<Pair<T, T>> {
    return this.flatMapIndexed { i, e ->
        this.subList(i + 1, this.size).map { e to it }
    }
}

fun <T> Set<T>.combinations2(): List<Pair<T, T>> = this.toList().combinations2()
