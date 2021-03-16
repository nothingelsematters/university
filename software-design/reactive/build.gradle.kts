import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.4.30"
}

group = "com.github.nothingelsematters"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    val SPRING_VERSION = "2.4.3"
    val JUPITER_VERSION = "5.8.0-M1"

    implementation("org.jetbrains.kotlin", "kotlin-reflect", "1.4.30")

    implementation("org.mongodb", "mongodb-driver-reactivestreams", "4.2.2")

    implementation("org.springframework.boot", "spring-boot-starter-webflux", SPRING_VERSION)
    implementation("org.springframework.boot", "spring-boot-starter-data-mongodb", SPRING_VERSION)

    implementation("com.natpryce", "konfig", "1.6.10.0")

    testImplementation(kotlin("test-junit5"))
    testImplementation("org.junit.jupiter", "junit-jupiter-api", JUPITER_VERSION)
    testRuntimeOnly("org.junit.jupiter", "junit-jupiter-engine", JUPITER_VERSION)

    testImplementation("org.springframework.boot", "spring-boot-starter-test", SPRING_VERSION)
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}
