package com.github.nothingelsematters.bridge.graph

import com.github.nothingelsematters.bridge.drawing.DrawingApi
import com.github.nothingelsematters.bridge.drawing.Point
import java.lang.Integer.min
import kotlin.math.cos
import kotlin.math.PI
import kotlin.math.sin

data class Vertex(val name: String)

abstract class AbstractGraph(private val drawingApi: DrawingApi) {

    abstract fun addEdge(fromVertex: Vertex, toVertex: Vertex)

    /**
     * @return a pair of vertices amount and a list of pairs of vertex edge indices
     */
    abstract fun getScheme(): Pair<Int, List<Pair<Int, Int>>>

    fun draw() {
        val (verticesAmount, edges) = getScheme()

        val maxWidth = drawingApi.drawingAreaWidth
        val maxHeight = drawingApi.drawingAreaHeight
        val center = Point(maxWidth.toDouble() / 2, maxHeight.toDouble() / 2)

        val minDimension = min(maxWidth, maxHeight)
        val radius = minDimension * 0.4

        val vertexCoordinates = List(verticesAmount) {
            val ith = it * 2 * PI / verticesAmount
            val point = Point(radius * cos(ith), radius * sin(ith)) + center
            drawingApi.drawCircle(point, minDimension * 0.02)
            point
        }

        for ((from, to) in edges) {
            drawingApi.drawLine(vertexCoordinates[from], vertexCoordinates[to])
        }

        drawingApi.print()
    }
}
