package com.github.nothingelsematters.bridge.drawing

data class Point(val x: Double, val y: Double) {
    operator fun plus(point: Point): Point = Point(x + point.x, y + point.y)
}

interface DrawingApi {

    val drawingAreaWidth: Int

    val drawingAreaHeight: Int

    fun drawCircle(center: Point, radius: Double)

    fun drawLine(startingPoint: Point, finishingPoint: Point)

    fun print()
}
