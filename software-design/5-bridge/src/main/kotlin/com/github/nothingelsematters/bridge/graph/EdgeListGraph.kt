package com.github.nothingelsematters.bridge.graph

import com.github.nothingelsematters.bridge.drawing.DrawingApi

private typealias Edge = Pair<Vertex, Vertex>

class EdgeListGraph(drawingApi: DrawingApi) : AbstractGraph(drawingApi) {

    private val vertexMap = mutableMapOf<Vertex, Int>()

    private val edges = mutableListOf<Edge>()

    private fun addVertexIfAbsent(vertex: Vertex) {
        vertexMap.computeIfAbsent(vertex) { vertexMap.size }
    }

    override fun addEdge(fromVertex: Vertex, toVertex: Vertex) {
        addVertexIfAbsent(fromVertex)
        addVertexIfAbsent(toVertex)
        edges.add(Edge(fromVertex, toVertex))
    }

    override fun getScheme(): Pair<Int, List<Pair<Int, Int>>> =
        vertexMap.size to edges.map { (from, to) -> vertexMap[from]!! to vertexMap[to]!! }
}
