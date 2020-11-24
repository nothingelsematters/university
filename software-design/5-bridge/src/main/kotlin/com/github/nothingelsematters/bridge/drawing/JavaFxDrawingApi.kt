package com.github.nothingelsematters.bridge.drawing

import javafx.application.Application
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.shape.Circle
import javafx.scene.shape.Line
import javafx.scene.shape.Shape
import javafx.stage.Stage

class JavaFxDrawingApi : DrawingApi, Application() {

    override val drawingAreaWidth = 600

    override val drawingAreaHeight = 600

    companion object {
        private val shapes: MutableList<Shape> = ArrayList()
    }

    override fun drawCircle(center: Point, radius: Double) {
        shapes.add(Circle(center.x, center.y, radius))
    }

    override fun drawLine(startingPoint: Point, finishingPoint: Point) {
        shapes.add(Line(startingPoint.x, startingPoint.y, finishingPoint.x, finishingPoint.y))
    }

    override fun start(stage: Stage?) {
        stage?.run {
            title = "JAVAFX"
            scene = Scene(Group(shapes.map { Group(it) }), drawingAreaWidth.toDouble(), drawingAreaHeight.toDouble())
            show()
        }
    }

    override fun print() = launch(this.javaClass)
}
