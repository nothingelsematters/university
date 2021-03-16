import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.4.31"
}

group = "com.github.nothingelsematters"
version = "1.0-SNAPSHOT"

repositories {
    maven(url = "https://jitpack.io")
    mavenCentral()
    jcenter()
}

val springVersion = "2.4.3"
val exposedVersion = "0.29.1"
val jupiterVersion = "5.8.0-M1"
val testContainersVersion = "1.15.1"

dependencies {
    implementation("org.jetbrains.kotlin", "kotlin-reflect", "1.4.31")

    implementation("org.springframework.boot", "spring-boot-starter-webflux", springVersion)

    implementation("org.jetbrains.exposed", "exposed-core", exposedVersion)
    implementation("org.jetbrains.exposed", "exposed-dao", exposedVersion)
    implementation("org.jetbrains.exposed", "exposed-jdbc", exposedVersion)
    implementation("org.jetbrains.exposed", "exposed-jodatime", exposedVersion)

    runtimeOnly("org.postgresql", "postgresql", "42.2.2")

    testImplementation(kotlin("test-junit5"))
    testImplementation("org.junit.jupiter", "junit-jupiter-api", jupiterVersion)
    testRuntimeOnly("org.junit.jupiter", "junit-jupiter-engine", jupiterVersion)

    testImplementation("com.tngtech.archunit", "archunit-junit5", "0.17.0")

    testImplementation("org.testcontainers", "postgresql", testContainersVersion)
    testImplementation("org.testcontainers", "junit-jupiter", testContainersVersion)

    testImplementation("org.springframework.boot", "spring-boot-starter-test", springVersion)
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}
