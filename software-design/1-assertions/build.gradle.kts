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
    testImplementation("org.jetbrains.kotlin:kotlin-test-junit:1.4.0")
}
