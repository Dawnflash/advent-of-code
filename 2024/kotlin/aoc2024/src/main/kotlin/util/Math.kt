package cz.dawnflash.aoc2024.util

// gcd, a1, b1: a1 * a + b1 * b = gcd(a, b)
fun eea(a: Long, b: Long): Triple<Long, Long, Long> {
    if (a == 0L) return Triple(b, 0, 1)
    val (gcd, a1, b1) = eea(b % a, a)
    return Triple(gcd, b1 - b / a * a1, a1)
}

fun gcd(a: Long, b: Long) = eea(a, b).first
fun lcm(a: Long, b: Long) = a / gcd(a, b) * b