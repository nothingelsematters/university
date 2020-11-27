package com.github.nothingelsematters.bridge.drawing

import java.awt.*
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D
import kotlin.system.exitProcess

class AwtDrawingApi : DrawingApi, Frame() {

    override val drawingAreaWidth = 600

    override val drawingAreaHeight = 600

    private val circles = mutableListOf<Ellipse2D.Double>()

    private val lines = mutableListOf<Line2D.Double>()

    override fun paint(graphics: Graphics) {
        val graphics2D = graphics as Graphics2D
        graphics2D.paint = Color.black
        circles.forEach(graphics2D::fill)
        lines.forEach(graphics2D::draw)
    }

    override fun drawCircle(center: Point, radius: Double) {
        circles.add(Ellipse2D.Double(center.x - radius, center.y - radius, radius * 2, radius * 2))
    }

    override fun drawLine(startingPoint: Point, finishingPoint: Point) {
        lines.add(Line2D.Double(startingPoint.x, startingPoint.y, finishingPoint.x, finishingPoint.y))
    }

    override fun print() {
        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(we: WindowEvent) {
                exitProcess(0)
            }
        })
        setSize(drawingAreaWidth, drawingAreaHeight)
        isVisible = true
    }
}
