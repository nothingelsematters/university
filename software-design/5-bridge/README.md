# Homework 5

> Goal : to gain practical experience in applying the structural pattern.

It is necessary to implement a simple graph visualiser using two different graphs
API. You can choose how you want to visualise the graph yourself (e.g. draw
tops in a circle). The application shall support two implementations of columns: on edge list and the adjacency matrix.

```java
public abstract class Graph {

    /**
    * Bridge to drawing api
    */
    private DrawingApi drawingApi;

    public Graph(DrawingApi drawingApi) {
        this.drawingApi = drawingApi;
    }

    public abstract void drawGraph();
}

public interface DrawingApi {
    long getDrawingAreaWidth();
    long getDrawingAreaHeight();
    void drawCircle(...);
    void drawLine(...);
}
```

Comments:

+ API selection and implementation of the column must be defined through command line arguments
when launching the application;
+ the class framework can be changed (add new fields/methods, method parameters, etc.);
+ java.awt and javafx can be used as [examples](https://github.com/akirakozov/software-design/tree/master/java/graphics/);
+ you can use any language and any api to draw (the most important thing is that they are
fundamentally different).
