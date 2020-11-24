package com.github.nothingelsematters.bridge

import com.github.nothingelsematters.bridge.drawing.AwtDrawingApi
import com.github.nothingelsematters.bridge.drawing.DrawingApi
import com.github.nothingelsematters.bridge.drawing.JavaFxDrawingApi
import com.github.nothingelsematters.bridge.graph.AbstractGraph
import com.github.nothingelsematters.bridge.graph.AdjacencyMatrixGraph
import com.github.nothingelsematters.bridge.graph.EdgeListGraph
import com.github.nothingelsematters.bridge.graph.Vertex
import kotlin.system.exitProcess

fun printUsageAndExit(): Nothing {
    println("Usage: --fx/--awt --list/--matrix")
    exitProcess(-1)
}

fun main(args: Array<String>) {

    val drawingApi: DrawingApi = when {
        "--fx" in args -> JavaFxDrawingApi()
        "--awt" in args -> AwtDrawingApi()
        else -> printUsageAndExit()
    }

    val graph: AbstractGraph = when {
        "--list" in args -> EdgeListGraph(drawingApi)
        "--matrix" in args -> AdjacencyMatrixGraph(drawingApi)
        else -> printUsageAndExit()
    }

    val vertices = List(5) { Vertex("vertex $it") }
    graph.addEdge(vertices[0], vertices[1])
    graph.addEdge(vertices[1], vertices[2])
    graph.addEdge(vertices[1], vertices[3])
    graph.addEdge(vertices[1], vertices[4])
    graph.addEdge(vertices[2], vertices[4])
    graph.addEdge(vertices[4], vertices[0])
    graph.addEdge(vertices[4], vertices[1])
    graph.addEdge(vertices[4], vertices[3])

    graph.draw()
}
