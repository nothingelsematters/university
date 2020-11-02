import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    id("org.springframework.boot") version "2.2.2.RELEASE"
    id("io.spring.dependency-management") version "1.0.8.RELEASE"
    kotlin("jvm") version "1.4.10"
    kotlin("plugin.spring") version "1.4.10"
    kotlin("plugin.allopen") version "1.4.10"
    kotlin("plugin.jpa") version "1.4.10"
    kotlin("kapt") version "1.4.10"
}

group = "com.github.nothingelsematters"
version = "1.0-SNAPSHOT"
java.sourceCompatibility = JavaVersion.VERSION_1_8

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlin", "kotlin-reflect")
    implementation("org.jetbrains.kotlin", "kotlin-stdlib-jdk8")

    implementation("io.github.microutils", "kotlin-logging", "2.0.3")

    implementation("org.springframework.boot", "spring-boot-starter-data-jpa")
    implementation("org.springframework.boot", "spring-boot-starter-mustache")
    implementation("org.springframework.boot", "spring-boot-starter-web")
    implementation("com.fasterxml.jackson.module", "jackson-module-kotlin")

    runtimeOnly("com.h2database", "h2")
    runtimeOnly("org.springframework.boot", "spring-boot-devtools")

    kapt("org.springframework.boot", "spring-boot-configuration-processor")
    testImplementation("org.springframework.boot", "spring-boot-starter-test") {
        exclude(group = "org.junit.vintage", module = "junit-vintage-engine")
        exclude(module = "mockito-core")
    }
    testImplementation("org.junit.jupiter", "junit-jupiter-api")
    testImplementation("com.ninja-squad", "springmockk", "1.1.3")
    testRuntimeOnly("org.junit.jupiter", "junit-jupiter-engine")
}

tasks.withType<Test> {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
    kotlinOptions {
        freeCompilerArgs = listOf("-Xjsr305=strict")
        jvmTarget = "1.8"
    }
}
