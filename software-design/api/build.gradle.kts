plugins {
    id("java")
    id("org.jetbrains.kotlin.jvm") version "1.4.0"
}

group = "com.github.nothingelsematters"
version = "0.0.1-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")

    // JSON parsing
    implementation("com.beust:klaxon:5.4")

    // logging
    implementation("io.github.microutils:kotlin-logging:2.0.3")
    implementation("org.slf4j:slf4j-simple:2.0.0-alpha1")

    // configuration
    implementation("com.natpryce:konfig:1.6.10.0")


    testImplementation("org.jetbrains.kotlin:kotlin-test-junit:1.4.0")
    testImplementation("io.mockk:mockk:1.10.2")
    testImplementation("com.xebialabs.restito:restito:0.8.2")
}
