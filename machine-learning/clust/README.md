# Clustering

## Goals

+ Implement a clustering algorithm and two quality measures.
+ Implement a dimensionality reduction algorithm.
+ Analysis of results.

## Task

Implement any clustering algorithm of your choice:

+ K-Means;
+ DBSCAN;
+ Hierarchical algorithm.

Also implement two clustering quality metrics: one external and one internal.
The internal quality measure should take into account both inter-cluster and intra-cluster distances.
[Outer metrics variant](http://neerc.ifmo.ru/wiki/index.php?title=%D0%9E%D1%86%D0%B5%D0%BD%D0%BA%D0%B0_%D0%BA%D0%B0%D1%87%D0%B5%D1%81%D1%82%D0%B2%D0%B0_%D0%B2_%D0%B7%D0%B0%D0%B4%D0%B0%D1%87%D0%B5_%D0%BA%D0%BB%D0%B0%D1%81%D1%82%D0%B5%D1%80%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8)

+ Rand index
+ Jaccard index
+ F-measure

Options for internal measures:

+ Silhouette index
+ Calinski-Harabasz metric
+ Dunn metric

Draw two graphs: dataset with real labels and with labels from clustering. Try to choose such hyperparameters of clustering algorithm to make clustering results as similar as possible to the real-world labels. To render multidimensional data, use a dimensionality reduction algorithm (e.g. PCA or tSNE), but clustering is still done in multidimensional space.

Plot the selected clustering quality metrics against the number of clusters if K-Means or hierarchical algorithm is chosen. In the case of DBSCAN, plot the dependence of selected clustering quality metrics on the radius of the ball.

## Data sets

Take any dataset from first KNN lab work. Don't forget to vectorize and normalize the dataset.
