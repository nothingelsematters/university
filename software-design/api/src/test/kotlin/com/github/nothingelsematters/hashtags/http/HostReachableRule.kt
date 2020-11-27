package com.github.nothingelsematters.hashtags.http

import org.junit.Assume
import org.junit.rules.TestRule
import org.junit.runner.Description
import org.junit.runners.model.Statement
import java.io.IOException
import java.util.concurrent.TimeUnit


class HostReachableRule : TestRule {
    @Retention(AnnotationRetention.RUNTIME)
    @Target(
        AnnotationTarget.FUNCTION,
        AnnotationTarget.PROPERTY_GETTER,
        AnnotationTarget.PROPERTY_SETTER,
        AnnotationTarget.ANNOTATION_CLASS,
        AnnotationTarget.CLASS
    )
    annotation class HostReachable(val value: String)

    override fun apply(statement: Statement, description: Description): Statement {
        val hostReachable = description.getAnnotation(HostReachable::class.java)

        return when {
            hostReachable == null -> statement
            !checkHost(hostReachable.value) -> SkipStatement(hostReachable.value)
            else -> statement
        }
    }

    private class SkipStatement(private val host: String) : Statement() {
        override fun evaluate() {
            Assume.assumeTrue("Skipped, because following host is not available at the moment: $host", false)
        }
    }

    companion object {
        private const val TIMEOUT = 1000L

        private fun checkHost(host: String) = nativePing(host) || nativePing6(host)

        private fun nativePing(host: String) = nativePingImpl("ping", host)

        private fun nativePing6(host: String) = nativePingImpl("ping6", host)

        private fun nativePingImpl(cmd: String, host: String) =
            try {
                val pingProcess = ProcessBuilder(cmd, "-c", "1", host).start()
                pingProcess.waitFor(TIMEOUT, TimeUnit.MILLISECONDS) || pingProcess.exitValue() == 0
            } catch (e: IOException) {
                e.printStackTrace()
                false
            } catch (e: InterruptedException) {
                e.printStackTrace()
                false
            }
    }
}