package com.github.nothingelsematters.bridge.graph

import com.github.nothingelsematters.bridge.drawing.DrawingApi

class AdjacencyMatrixGraph(drawingApi: DrawingApi) : AbstractGraph(drawingApi) {

    private val vertices = mutableListOf<Vertex>()

    private val vertexIndex = mutableMapOf<Vertex, Int>()

    private val edges = mutableListOf<MutableList<Int>>()

    private fun addVertexIfAbsent(vertex: Vertex): Int =
        vertexIndex.computeIfAbsent(vertex) { it ->
            vertices.add(it)
            edges.forEach { list -> list.add(0) }
            edges.add(MutableList(edges.size + 1) { 0 })
            vertices.lastIndex
        }

    override fun addEdge(fromVertex: Vertex, toVertex: Vertex) {
        val fromIndex = addVertexIfAbsent(fromVertex)
        val toIndex = addVertexIfAbsent(toVertex)
        edges[fromIndex][toIndex]++
    }

    override fun getScheme(): Pair<Int, List<Pair<Int, Int>>> =
        vertices.size to edges.flatMapIndexed { fromIndex, mutableList ->
            mutableList.flatMapIndexed { toIndex, amount ->
                List(amount) { fromIndex to toIndex }
            }
        }
}
