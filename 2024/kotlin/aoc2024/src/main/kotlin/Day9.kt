package cz.dawnflash.aoc2024

class Day9 : Day() {
    override val sampleChecks = "1928" to "2858"
    override val checks = "6461289671426" to "6488291456470"

    data class Block(var size: Int, var offset: Int)

    private fun checksum(disk: Array<Int?>): ULong = disk.foldIndexed(0UL) { index, acc, i ->
        acc + index.toULong() * (i ?: 0).toULong()
    }

    private fun checksum(files: List<Block>) = files.foldIndexed(0UL) { index, acc, i ->
        acc + index.toULong() * (i.offset.toULong()..<(i.offset + i.size).toULong()).sum()
    }

    private fun defragment(disk: Array<Int?>) {
        var l = 0
        var r = disk.size - 1
        while (l < r) {
            when {
                disk[l] != null -> l++
                disk[r] == null -> r--
                else -> {
                    disk[l] = disk[r]
                    disk[r] = null
                    l++
                    r--
                }
            }
        }
    }

    // this clobbers the voids (it doesn't create new voids at the end) but who cares
    private fun defragment(files: List<Block>, voids: List<Block>) {
        for (f in files.reversed()) {
            val v = voids.find { it.size >= f.size && it.offset < f.offset }
            if (v == null) continue
            f.offset = v.offset
            v.size -= f.size
            v.offset += f.size
        }
    }

    override fun solution1(input: List<String>): String {
        val disk = input[0].flatMapIndexed { i, c ->
            val size = c.digitToInt(); when {
            i % 2 == 0 -> List(size) { i / 2 }
            else -> List(size) { null }
        }
        }.toTypedArray()
        defragment(disk)

        return checksum(disk).toString()
    }

    override fun solution2(input: List<String>): String {
        var offset = 0
        val blocks: List<Block> = input[0].map {
            val size = it.digitToInt()
            Block(size, offset).also { offset += size }
        }
        val files = blocks.filterIndexed { i, _ -> i % 2 == 0 }
        val voids = blocks.filterIndexed { i, _ -> i % 2 != 0 }

        defragment(files, voids)

        return checksum(files).toString()
    }
}