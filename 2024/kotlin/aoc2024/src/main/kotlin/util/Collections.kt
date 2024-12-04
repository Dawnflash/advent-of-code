package cz.dawnflash.aoc2024.util

public fun <T> List<List<T>>.transpose(): List<List<T>> {
    return this[0].indices.map { i -> this.indices.map { j -> this[j][i] } }
}

@JvmName("listStringTranspose")
public fun List<String>.transpose(): List<String> {
    return this.indices.map { j -> this[0].indices.map { i -> this[i][j] }.joinToString("")}
}