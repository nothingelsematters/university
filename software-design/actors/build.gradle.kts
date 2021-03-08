import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.4.30"
    application
}

group = "com.github.nothingelsematters"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    jcenter()
}

dependencies {
    implementation("org.jetbrains.kotlin", "kotlin-reflect", "1.4.30")

    implementation("com.typesafe.akka", "akka-actor_2.13", "2.6.12")
    implementation("khttp", "khttp", "1.0.0")

    runtimeOnly("org.slf4j", "slf4j-api", "2.0.0-alpha1")
    runtimeOnly("org.slf4j", "slf4j-simple", "2.0.0-alpha1")

    testImplementation(kotlin("test-junit5"))
    testImplementation("org.junit.jupiter", "junit-jupiter-api", "5.6.0")
    testRuntimeOnly("org.junit.jupiter", "junit-jupiter-engine", "5.6.0")

    testImplementation("com.xebialabs.restito", "restito", "0.8.2")

    testImplementation("com.fasterxml.jackson.core", "jackson-databind", "2.12.1")
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile>() {
    kotlinOptions.jvmTarget = "1.8"
}

application {
    mainClassName = "MainKt"
}
