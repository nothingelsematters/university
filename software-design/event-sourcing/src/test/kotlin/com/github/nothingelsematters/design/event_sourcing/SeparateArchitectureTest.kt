package com.github.nothingelsematters.design.event_sourcing

import com.tngtech.archunit.core.domain.JavaClass.Predicates.resideInAPackage
import com.tngtech.archunit.core.domain.JavaClass.Predicates.resideOutsideOfPackages
import com.tngtech.archunit.junit.AnalyzeClasses
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes
import com.tngtech.archunit.junit.ArchTest

@AnalyzeClasses(packages = ["com.github.nothingelsematters.design.event_sourcing"])
class SeparateArchitectureTest {

    private val rootPackage = "com.github.nothingelsematters.design.event_sourcing"

    private val commonPackage = "$rootPackage.common"

    @ArchTest
    private val `everything depends only on common test` =
        classes()
            .that()
            .resideOutsideOfPackage(commonPackage)
            .should()
            .onlyDependOnClassesThat(
                resideInAPackage(commonPackage)
                    .or(resideOutsideOfPackages(rootPackage))
                    .or(resideInAPackage(rootPackage))
            )
}
