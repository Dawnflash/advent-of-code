plugins {
    kotlin("jvm") version "2.0.21"
    application
}

group = "cz.dawnflash"

version = "1.0-SNAPSHOT"

repositories { mavenCentral() }

dependencies {
    testImplementation(kotlin("test"))
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.9.0")
    implementation(kotlin("reflect"))
}

tasks.test { useJUnitPlatform() }

application {
    mainClass = "cz.dawnflash.aoc2024.MainKt"
}
