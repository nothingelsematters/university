package com.github.nothingelsematters.aop

import org.aspectj.lang.ProceedingJoinPoint
import org.aspectj.lang.annotation.*
import org.springframework.core.annotation.Order
import org.springframework.stereotype.Component

@Aspect
@Component
@Order(1)
object StatisticsAspect {

    val executionCount = mutableMapOf<String, Int>()

    val executionTime = mutableMapOf<String, Long>()

    @Around("execution(* com.github.nothingelsematters.aop.Controller.*(..))")
    fun aroundFindAccountsAdvice(processingJoinPoint: ProceedingJoinPoint): Any {
        val signature = processingJoinPoint.signature.toLongString()
        executionCount[signature] = executionCount.getOrDefault(signature, 0) + 1

        val begin = System.currentTimeMillis()
        val result = processingJoinPoint.proceed()
        val end = System.currentTimeMillis()

        executionTime[signature] = executionTime.getOrDefault(signature, 0) + end - begin

        println("""
            $signature:
                + called: ${executionCount[signature]}
                + lasted: ${executionTime[signature]}
                + mean  : ${executionTime[signature]!! / executionCount[signature]!!}
        """.trimIndent())

        return result
    }
}
