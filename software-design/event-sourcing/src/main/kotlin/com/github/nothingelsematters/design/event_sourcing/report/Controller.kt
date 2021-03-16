package com.github.nothingelsematters.design.event_sourcing.report

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/statistics")
class Controller(private val reportService: ReportService) {

    @GetMapping("days") fun dayStatistics(@RequestBody visitingStatisticsRequest: VisitingStatisticsRequest) =
        reportService.getVisitingStatistics(visitingStatisticsRequest.from, visitingStatisticsRequest.to)

    @GetMapping("average") fun averageStatistics() = reportService.getAverageStatistics()
}
